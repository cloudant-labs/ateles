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
    nullptr,
    JS_GlobalObjectTraceHook};

/* The class of the global object. */
static JSClass global_class = {"global", JSCLASS_GLOBAL_FLAGS, &global_ops};

static bool
print_fun(JSContext* cx, unsigned argc, JS::Value* vp)
{
    JS::CallArgs args = CallArgsFromVp(argc, vp);

    for(int i = 0; i < args.length(); i++) {
        fprintf(stderr, "> %s\n", format_value(cx, args[i]).c_str());
    }

    return true;
}

JSContext*
create_context()
{
    JSContext* cx = JS_NewContext(8L * 1024 * 1024);
    if(cx == nullptr) {
        throw AtelesInternalError("Error creating JavaScript context.");
    }
    if(!JS::InitSelfHostedCode(cx)) {
        throw AtelesInternalError("Error initializing self hosted code.");
    }
    return cx;
}

Context::Context() : _cx(create_context(), JS_DestroyContext)
{
    JSAutoRequest ar(this->_cx.get());

    JS::CompartmentOptions options;
    this->_conv_global = new JS::RootedObject(this->_cx.get(),
        JS_NewGlobalObject(this->_cx.get(),
            &global_class,
            nullptr,
            JS::FireOnNewGlobalHook,
            options));

    if(this->_conv_global == nullptr) {
        throw AtelesInternalError("Error allocating conversion global object.");
    }

    this->_global = new JS::RootedObject(this->_cx.get(),
        JS_NewGlobalObject(this->_cx.get(),
            &global_class,
            nullptr,
            JS::FireOnNewGlobalHook,
            options));

    if(this->_global == nullptr) {
        throw AtelesInternalError("Error allocating global object.");
    }

    {
        // Scope to the conv compartment
        JSAutoCompartment ac(this->_cx.get(), *this->_conv_global);
        JS_InitStandardClasses(this->_cx.get(), *this->_conv_global);

        load_script(this->_cx.get(),
            "esprima.js",
            std::string((char*) esprima_data, esprima_len));
        load_script(this->_cx.get(),
            "escodegen.js",
            std::string((char*) escodegen_data, escodegen_len));
        load_script(this->_cx.get(),
            "rewrite_anon_fun.js",
            std::string((char*) rewrite_anon_fun_data, rewrite_anon_fun_len));
    }

    {
        // Scope to the map compartment
        JSAutoCompartment ac(this->_cx.get(), *this->_global);
        JS_InitStandardClasses(this->_cx.get(), *this->_global);

        load_script(this->_cx.get(),
            "ateles_map.js",
            std::string((char*) ateles_map_data, ateles_map_len));

        JS::HandleObject global_obj(this->_global);
        if(!JS_DefineFunction(
               this->_cx.get(), global_obj, "print", print_fun, 1, 0)) {
            throw AtelesInternalError("Error installing print function.");
        }
    }
}

std::string
Context::set_lib(const std::string& lib)
{
    return "";
}

std::string
Context::add_map_fun(const std::string& id, const std::string& source)
{
    std::string conv = this->transpile(source);

    JSAutoRequest ar(this->_cx.get());
    JSAutoCompartment ac(this->_cx.get(), *this->_global);

    JS::HandleObject this_obj(this->_global);
    JS::AutoValueArray<2> argv(this->_cx.get());
    argv[0].setString(string_to_js(this->_cx.get(), id));
    argv[1].setString(string_to_js(this->_cx.get(), conv));

    JS::RootedValue rval(this->_cx.get());

    if(!JS::Call(this->_cx.get(), this_obj, "addFun", argv, &rval)) {
        JS::RootedValue exc(this->_cx.get());
        if(!JS_GetPendingException(this->_cx.get(), &exc)) {
            throw AtelesInternalError(
                "Unknown error when adding map function.");
        } else {
            JS_ClearPendingException(this->_cx.get());
            throw AtelesInvalidArgumentError("Error adding map function: "
                + format_exception(this->_cx.get(), exc));
        }
    }

    return "";
}

std::string
Context::map_doc(const std::string& doc)
{
    JSAutoRequest ar(this->_cx.get());
    JSAutoCompartment ac(this->_cx.get(), *this->_global);

    JS::HandleObject this_obj(this->_global);

    JSString* js_doc = string_to_js(this->_cx.get(), doc);

    JS::AutoValueArray<1> argv(this->_cx.get());
    argv[0].setString(js_doc);

    JS::RootedValue rval(this->_cx.get());

    if(!JS::Call(this->_cx.get(), this_obj, "mapDoc", argv, &rval)) {
        JS::RootedValue exc(this->_cx.get());
        if(!JS_GetPendingException(this->_cx.get(), &exc)) {
            throw AtelesInternalError("Unknown mapping document.");
        } else {
            JS_ClearPendingException(this->_cx.get());
            throw AtelesInvalidArgumentError("Error mapping document: "
                + format_exception(this->_cx.get(), exc));
        }
    }

    return js_to_string(this->_cx.get(), rval);
}

std::string
Context::transpile(const std::string& source)
{
    JSAutoRequest ar(this->_cx.get());
    JSAutoCompartment ac(this->_cx.get(), *this->_conv_global);

    JS::HandleObject this_obj(this->_conv_global);
    JS::AutoValueArray<1> argv(this->_cx.get());
    argv[0].setString(string_to_js(this->_cx.get(), source));
    JS::RootedValue rval(this->_cx.get());

    if(!JS::Call(this->_cx.get(), this_obj, "rewriteAnonFun", argv, &rval)) {
        JS::RootedValue exc(this->_cx.get());
        if(!JS_GetPendingException(this->_cx.get(), &exc)) {
            throw AtelesInternalError(
                "Unknown error converting anonymous JavaScript function.");
        } else {
            JS_ClearPendingException(this->_cx.get());
            throw AtelesInvalidArgumentError("Invalid JavaScript function: "
                + format_exception(this->_cx.get(), exc));
        }
    }

    return js_to_string(this->_cx.get(), rval);
}

}  // namespace ateles