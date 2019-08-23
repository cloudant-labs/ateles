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
typedef std::promise<std::string> JSPromise;
typedef std::future<std::string> JSFuture;

class Context {
  public:
    explicit Context();

    std::string set_lib(const std::string& lib);
    std::string add_map_fun(const std::string& id, const std::string& source);
    std::string map_doc(const std::string& doc);

  private:
    std::string transpile(const std::string& source);

    std::unique_ptr<JSContext, void (*)(JSContext*)> _cx;
    JS::RootedObject* _conv_global;
    JS::RootedObject* _global;
};

std::string js_to_string(JSContext* cx, JS::HandleValue val);
JSString* string_to_js(JSContext* cx, const std::string& s);
std::string format_string(JSContext* cx, JS::HandleString str);
std::string format_value(JSContext* cx, JS::HandleValue val);
std::string format_exception(JSContext* cx, JS::HandleValue exc);
void load_script(JSContext* cx, std::string name, std::string source);

}  // namespace ateles

#endif  // included js.h