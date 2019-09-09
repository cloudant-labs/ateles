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

#include "ateles_map.h"
#include "errors.h"
#include "escodegen.h"
#include "esprima.h"
#include "rewrite_anon_fun.h"

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
create_jscontext()
{
    JSContext* cx = JS_NewContext(128L * 1024 * 1024);

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


JSCx::JSCx() : _cx(create_jscontext(), JS_DestroyContext)
{
}


std::unique_ptr<JSCompartment>
JSCx::new_compartment()
{
    return std::make_unique<JSCompartment>(this->_cx.get());
}


JSCompartment::JSCompartment(JSContext* cx) : _cx(cx)
{
    JSAutoRequest ar(this->_cx);
    JS::CompartmentOptions options;

    JSObject* global = JS_NewGlobalObject(this->_cx,
        &global_class,
        nullptr,
        JS::FireOnNewGlobalHook,
        options);

    if(global == nullptr) {
        throw AtelesResourceExhaustedError("Unable to allocate new global object.");
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
            throw AtelesInternalError(
                "Unknown error evaluating script: " + file);
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
            throw AtelesInternalError(
                "Unknown calling function.");
        } else {
            JS_ClearPendingException(this->_cx);
            throw AtelesInvalidArgumentError("Error calling function: "
                + format_exception(this->_cx, exc));
        }
    }

    return format_value(this->_cx, rval);
}

}  // namespace ateles
