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

#include <sstream>

#include "errors.h"
#include "js.h"
#include "js/Conversions.h"

namespace ateles
{
enum class PrintErrorKind
{
    Error,
    Warning,
    StrictWarning,
    Note
};

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

}  // namespace ateles