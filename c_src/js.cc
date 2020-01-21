// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

#include "js.h"

#include <sstream>

#include "errors.h"
#include "js/Conversions.h"

#define ACTIVE_SLEEP_TIME_MSEC 100   // 100ms = 0.1s
#define MAX_SLEEP_TIME_MSEC INT_MAX  // A really long time ~25 days

std::string json_stringify(JSContext* cx, JS::HandleValue val);
std::string js_to_string(JSContext* cx, JS::HandleValue val);
JSString* string_to_js(JSContext* cx, const std::string& s);
std::string format_string(JSContext* cx, JS::HandleString str);
std::string format_value(JSContext* cx, JS::HandleValue val);
std::string format_exception(JSContext* cx, JS::HandleValue exc);
void load_script(JSContext* cx, std::string name, std::string source);

static JSClassOps global_ops = {nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    JS_GlobalObjectTraceHook};

/* The class of the global object. */
static JSClass global_class = {"global", JSCLASS_GLOBAL_FLAGS, &global_ops};

std::chrono::system_clock::duration
wait_time(int msec_wait)
{
    auto sec_wait = double(msec_wait) / 1000.0;
    return std::chrono::duration_cast<std::chrono::system_clock::duration>(
        std::chrono::duration<double>(sec_wait));
}

static bool
print_fun(JSContext* cx, unsigned argc, JS::Value* vp)
{
    JS::CallArgs args = CallArgsFromVp(argc, vp);

    for(unsigned i = 0; i < args.length(); i++) {
        fprintf(stderr, "> %s\n", format_value(cx, args[i]).c_str());
    }

    return true;
}

JSContext*
create_jscontext(size_t max_mem)
{
    JSContext* cx = JS_NewContext(max_mem * 1024L * 1024L);

    if(cx == nullptr) {
        throw AtelesInternalError("Error creating JavaScript context.");
    }

    JS::ContextOptionsRef(cx)
        .setBaseline(false)
        .setIon(false)
        .setAsmJS(false)
        .setWasm(false)
        .setWasmBaseline(false)
        .setWasmIon(false);

    if(!JS::InitSelfHostedCode(cx)) {
        throw AtelesInternalError("Error initializing self hosted code.");
    }

    return cx;
}

bool
check_timeout_shim(JSContext* jscx)
{
    JSCx* cx = (JSCx*) JS_GetContextPrivate(jscx);
    return cx->check_timeout();
}

JSCx::JSCx(size_t max_mem) :
    _cx_mgr(create_jscontext(max_mem), JS_DestroyContext),
    _wd_alive(true),
    _wd_active(false)
{
    this->_cx = this->_cx_mgr.get();

    JS_SetContextPrivate(this->_cx, this);
    JS_AddInterruptCallback(this->_cx, &check_timeout_shim);

    JSAutoRequest ar(this->_cx);

    JS::CompartmentOptions options;

    JSObject* global = JS_NewGlobalObject(
        this->_cx, &global_class, nullptr, JS::FireOnNewGlobalHook, options);

    assert(global != nullptr && "error allocating new JS context");

    this->_global.init(this->_cx, global);

    JSAutoCompartment ac(this->_cx, this->_global);
    JS_InitStandardClasses(this->_cx, this->_global);

    JS::HandleObject global_obj(this->_global);
    if(!JS_DefineFunction(this->_cx, global_obj, "print", print_fun, 1, 0)) {
        throw AtelesInternalError("Error installing print function.");
    }

    this->_wd_alive = true;
    this->_wd_active = false;
    this->_wd_thread = std::make_unique<std::thread>(&JSCx::wd_run, this);
}

JSCx::~JSCx()
{
    // Scope the lock acquisition
    {
        std::unique_lock<std::mutex> guard(this->_wd_lock);
        this->_wd_alive = false;
        this->_wd_cv.notify_one();
    }
    this->_wd_thread->join();
}

std::string
JSCx::eval(const std::string& script, std::vector<std::string>& args)
{
    JSAutoRequest ar(this->_cx);
    JSAutoCompartment ac(this->_cx, this->_global.get());

    JS_MaybeGC(this->_cx);

    // parse args as key=value pairs
    // for things like filename and line number
    // and maybe compile options eventually?

    std::string file = "<unknown>";
    unsigned line = 1;

    for(auto const& arg : args) {
        auto idx = arg.find("=");
        if(idx == std::string::npos) {
            throw AtelesInvalidArgumentError("Invalid option: " + arg);
        }
        std::string key = arg.substr(0, idx);
        std::string val = arg.substr(idx + 1);

        if(key == "file") {
            file = val;
        } else if(key == "line") {
            try {
                line = std::stoul(val);
            } catch(...) {
                throw AtelesInvalidArgumentError("Invalid line number: " + val);
            }
        } else {
            throw AtelesInvalidArgumentError("Unknown eval option: " + key);
        }
    }

    JS::RootedValue rval(this->_cx);
    JS::CompileOptions opts(this->_cx);
    opts.setFileAndLine(file.c_str(), line);
    if(!JS::Evaluate(this->_cx, opts, script.c_str(), script.size(), &rval)) {
        JS::RootedValue exc(this->_cx);
        if(!JS_GetPendingException(this->_cx, &exc)) {
            if(this->timed_out()) {
                throw AtelesTimeoutError("Time out evaluating script: " + file);
            } else {
                throw AtelesInternalError(
                    "Unknown error evaluating script: " + file);
            }
        } else {
            JS_ClearPendingException(this->_cx);
            throw AtelesInvalidArgumentError(
                "Error evaluating script: " + format_exception(this->_cx, exc));
        }
    }

    return json_stringify(this->_cx, rval);
}

std::string
JSCx::call(const std::string& name, std::vector<std::string>& args)
{
    JSAutoRequest ar(this->_cx);
    JSAutoCompartment ac(this->_cx, this->_global.get());

    JS_MaybeGC(this->_cx);

    JS::HandleObject this_obj(this->_global);
    JS::AutoValueVector jsargs(this->_cx);

    for(std::size_t i = 0; i < args.size(); i++) {
        auto str = JS::StringValue(string_to_js(this->_cx, args[i]));
        if(!jsargs.append(str)) {
            throw AtelesInternalError("Error creating arguments vector.");
        }
    }

    JS::RootedValue rval(this->_cx);

    if(!JS::Call(this->_cx, this_obj, name.c_str(), jsargs, &rval)) {
        JS::RootedValue exc(this->_cx);
        if(!JS_GetPendingException(this->_cx, &exc)) {
            if(this->timed_out()) {
                throw AtelesTimeoutError("Time out calling function: " + name);
            } else {
                throw AtelesInternalError("Unknown calling function.");
            }
        } else {
            JS_ClearPendingException(this->_cx);
            throw AtelesInvalidArgumentError(
                "Error calling function: " + format_exception(this->_cx, exc));
        }
    }

    return json_stringify(this->_cx, rval);
}

void
JSCx::set_timeout(int ms_timeout)
{
    this->_deadline = std::chrono::system_clock::now() + wait_time(ms_timeout);
    this->_timed_out = false;
}

bool
JSCx::check_timeout()
{
    if(std::chrono::system_clock::now() > this->_deadline) {
        this->_timed_out = true;
        return false;
    }

    return true;
}

bool
JSCx::timed_out()
{
    return this->_timed_out;
}

void
JSCx::set_watchdog_status(bool status)
{
    std::unique_lock<std::mutex> guard(this->_wd_lock);
    this->_wd_active = status;
    this->_wd_cv.notify_one();
}

void
JSCx::wd_run()
{
    std::unique_lock<std::mutex> guard(this->_wd_lock);
    std::chrono::system_clock::duration sleep_time;
    while(this->_wd_alive) {
        JS_RequestInterruptCallback(this->_cx);

        if(this->_wd_active) {
            sleep_time = wait_time(ACTIVE_SLEEP_TIME_MSEC);
        } else {
            sleep_time = wait_time(MAX_SLEEP_TIME_MSEC);
        }

        this->_wd_cv.wait_for(guard, sleep_time);
    }
}

JSCxAutoTimeout::JSCxAutoTimeout(JSCx* cx, int ms_timeout) : _cx(cx)
{
    this->_cx->set_timeout(ms_timeout);
    this->_cx->set_watchdog_status(true);
}

JSCxAutoTimeout::~JSCxAutoTimeout()
{
    this->_cx->set_watchdog_status(false);
}

enum class PrintErrorKind
{
    Error,
    Warning,
    StrictWarning,
    Note
};

static bool
json_accumulate(const char16_t* in, uint32_t len, void* obj)
{
    std::vector<char16_t>* out = (std::vector<char16_t>*) obj;
    out->insert(out->end(), in, in + len);
    return true;
}

std::string
json_stringify(JSContext* cx, JS::HandleValue val)
{
    JS::RootedValue tmp(cx, val);
    JS::RootedValue indent(cx, JS::NumberValue(2));
    std::vector<char16_t> buf;
    bool ok = JS_Stringify(cx, &tmp, nullptr, indent, json_accumulate, &buf);
    if(ok) {
        JSString* str = JS_NewUCStringCopyN(cx, &(buf[0]), buf.size());
        JS::RootedString rstr(cx, str);
        return format_string(cx, rstr);
    } else {
        return "<invalid_json>";
    }
}

std::string
js_to_string(JSContext* cx, JS::HandleValue val)
{
    JS::RootedString sval(cx);
    sval = val.toString();

    JS::UniqueChars chars(JS_EncodeStringToUTF8(cx, sval));
    if(!chars) {
        JS_ClearPendingException(cx);
        throw AtelesInvalidArgumentError("Error converting value to string.");
    }

    return chars.get();
}

JSString*
string_to_js(JSContext* cx, const std::string& s)
{
    JSString* ret = JS_NewStringCopyN(cx, s.c_str(), s.size());
    if(ret != nullptr) {
        return ret;
    }

    throw AtelesResourceExhaustedError("Unable to allocate string object.");
}

std::string
format_string(JSContext* cx, JS::HandleString str)
{
    std::string buf;

    JS::UniqueChars chars(JS_EncodeStringToUTF8(cx, str));
    if(!chars) {
        JS_ClearPendingException(cx);
        return "[invalid string]";
    }

    buf += chars.get();

    return buf;
}

std::string
format_value(JSContext* cx, JS::HandleValue val)
{
    JS::RootedString str(cx);

    if(val.isString()) {
        str = val.toString();
        return format_string(cx, str);
    }

    str = JS::ToString(cx, val);

    if(!str) {
        JS_ClearPendingException(cx);
        str = JS_ValueToSource(cx, val);
    }

    if(!str) {
        JS_ClearPendingException(cx);
        if(val.isObject()) {
            const JSClass* klass = JS_GetClass(&val.toObject());
            if(klass) {
                str = JS_NewStringCopyZ(cx, klass->name);
            } else {
                return "[uknown object]";
            }
        } else {
            return "[unknown non-object]";
        }
    }

    if(!str) {
        JS_ClearPendingException(cx);
        return "[invalid class]";
    }

    JS::UniqueChars bytes(JS_EncodeStringToUTF8(cx, str));
    if(!bytes) {
        JS_ClearPendingException(cx);
        return "[invalid string]";
    }

    return bytes.get();
}

std::string
format_exception(JSContext* cx, JS::HandleValue exc)
{
    if(!exc.isObject()) {
        return format_value(cx, exc);
    }

    JS::RootedObject exc_obj(cx, &exc.toObject());
    JSErrorReport* report = JS_ErrorFromException(cx, exc_obj);

    if(!report) {
        return format_value(cx, exc);
    }

    std::ostringstream prefix;
    if(report->filename) {
        prefix << report->filename << ':';
    }

    if(report->lineno) {
        prefix << report->lineno << ':' << report->column << ' ';
    }

    PrintErrorKind kind = PrintErrorKind::Error;
    if(JSREPORT_IS_WARNING(report->flags)) {
        if(JSREPORT_IS_STRICT(report->flags)) {
            kind = PrintErrorKind::StrictWarning;
        } else {
            kind = PrintErrorKind::Warning;
        }
    }

    if(kind != PrintErrorKind::Error) {
        const char* kindPrefix = nullptr;
        switch(kind) {
            case PrintErrorKind::Error:
                MOZ_CRASH("unreachable");
            case PrintErrorKind::Warning:
                kindPrefix = "warning";
                break;
            case PrintErrorKind::StrictWarning:
                kindPrefix = "strict warning";
                break;
            case PrintErrorKind::Note:
                kindPrefix = "note";
                break;
        }

        prefix << kindPrefix << ": ";
    }

    prefix << std::endl << report->message().c_str();

    return prefix.str();
}

void
load_script(JSContext* cx, std::string name, std::string source)
{
    JS::RootedValue rval(cx);
    JS::CompileOptions opts(cx);
    opts.setFileAndLine(name.c_str(), 1);
    if(!JS::Evaluate(cx, opts, source.c_str(), source.size(), &rval)) {
        JS::RootedValue exc(cx);
        if(!JS_GetPendingException(cx, &exc)) {
            throw AtelesInternalError(
                "Unknown error evaluating script: " + name);
        } else {
            JS_ClearPendingException(cx);
            throw AtelesInternalError(
                "Error evaluating script: " + format_exception(cx, exc));
        }
    }
}
