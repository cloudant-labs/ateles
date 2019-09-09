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

#ifndef ATELES_JS_H
#define ATELES_JS_H

#include <future>

#include "js/Initialization.h"
#include "jsapi.h"

namespace ateles
{

class JSCompartment;

class JSCx {
  public:
    typedef std::unique_ptr<JSCx> Ptr;

    explicit JSCx(size_t max_mem);

    std::unique_ptr<JSCompartment> new_compartment();

  private:
    std::unique_ptr<JSContext, void (*)(JSContext*)> _cx;
};


class JSCompartment {
public:
    typedef std::unique_ptr<JSCompartment> Ptr;

    explicit JSCompartment(JSContext* cx);
    ~JSCompartment();

    std::string eval(const std::string& script, std::vector<std::string>& args);
    std::string call(const std::string& name, std::vector<std::string>& args);

private:
    JSContext* _cx;
    JS::PersistentRootedObject _global;
};


std::string js_to_string(JSContext* cx, JS::HandleValue val);
JSString* string_to_js(JSContext* cx, const std::string& s);
std::string format_string(JSContext* cx, JS::HandleString str);
std::string format_value(JSContext* cx, JS::HandleValue val);
std::string format_exception(JSContext* cx, JS::HandleValue exc);
void load_script(JSContext* cx, std::string name, std::string source);

}  // namespace ateles

#endif  // included js.h