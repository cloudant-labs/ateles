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

#define ACTIVE_SLEEP_TIME 100000 // 100,000 us = 100ms = 0.1s
#define MAX_SLEEP_TIME INT_MAX // A really long time

namespace ateles
{
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


JSCx::JSCx(size_t max_mem) : _cx(create_jscontext(max_mem), JS_DestroyContext),
        _wd_alive(true), _wd_active(false)
{
    JS_SetContextPrivate(this->_cx.get(), this);
    JS_AddInterruptCallback(this->_cx.get(), &check_timeout_shim);

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


std::unique_ptr<JSCompartment>
JSCx::new_compartment()
{
    return std::make_unique<JSCompartment>(this, this->_cx.get());
}


void
JSCx::set_timeout(int ms_timeout)
{
    int us_timeout = ms_timeout * 1000;
    auto now = std::chrono::system_clock::now();
    this->_deadline = now + std::chrono::system_clock::duration(us_timeout);
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
        JS_RequestInterruptCallback(this->_cx.get());

        if(this->_wd_active) {
            sleep_time = std::chrono::system_clock::duration(ACTIVE_SLEEP_TIME);
        } else {
            sleep_time = std::chrono::system_clock::duration(MAX_SLEEP_TIME);
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


JSCompartment::JSCompartment(JSCx* jscx, JSContext* cx) : _jscx(jscx), _cx(cx)
{
    JSAutoRequest ar(this->_cx);

    JS::CompartmentOptions options;

    JSObject* global = JS_NewGlobalObject(this->_cx,
        &global_class,
        nullptr,
        JS::FireOnNewGlobalHook,
        options);

    if(global == nullptr) {
        // Force garbage collection and retry creating
        // the new compartment.
        JS_GC(this->_cx);

        global = JS_NewGlobalObject(this->_cx,
            &global_class,
            nullptr,
            JS::FireOnNewGlobalHook,
            options);

        if(global == nullptr) {
            throw AtelesResourceExhaustedError("Unable to allocate new global object.");
        }
    }

    this->_global.init(this->_cx, global);

    JSAutoCompartment ac(this->_cx, this->_global.get());
    JS_InitStandardClasses(this->_cx, this->_global);

    JS::HandleObject global_obj(this->_global);
    if(!JS_DefineFunction(
           this->_cx, global_obj, "print", print_fun, 1, 0)) {
        throw AtelesInternalError("Error installing print function.");
    }
}


JSCompartment::~JSCompartment()
{
    this->_global.reset();
}


std::string
JSCompartment::eval(const std::string& script, std::vector<std::string>& args)
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
            if(this->_jscx->timed_out()) {
                throw AtelesTimeoutError(
                    "Time out evaluating script: " + file);
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

    return format_value(this->_cx, rval);
}


std::string
JSCompartment::call(const std::string& name, std::vector<std::string>& args)
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
            if(this->_jscx->timed_out()) {
                throw AtelesTimeoutError(
                    "Time out calling function: " + name);
            } else {
                throw AtelesInternalError(
                    "Unknown calling function.");
            }
        } else {
            JS_ClearPendingException(this->_cx);
            throw AtelesInvalidArgumentError("Error calling function: "
                + format_exception(this->_cx, exc));
        }
    }

    return format_value(this->_cx, rval);
}

}  // namespace ateles
