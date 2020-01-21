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

#include <condition_variable>
#include <future>
#include <mutex>
#include <vector>

#include "errors.h"
#include "jsapi.h"

class JSCxAutoTimeout;

class JSCx {
  public:
    typedef std::unique_ptr<JSCx> Ptr;

    explicit JSCx(size_t max_mem);
    ~JSCx();

    std::string eval(const std::string& script, std::vector<std::string>& args);
    std::string call(const std::string& name, std::vector<std::string>& args);
    void set_timeout(int ms_timeout);
    bool check_timeout();
    bool timed_out();

    void set_watchdog_status(bool status);

  private:
    JSContext* _cx;
    std::unique_ptr<JSContext, void (*)(JSContext*)> _cx_mgr;
    JS::PersistentRootedObject _global;

    // Watchdog setup for capping script execution time
    void wd_run();
    std::unique_ptr<std::thread> _wd_thread;
    std::mutex _wd_lock;
    std::condition_variable _wd_cv;
    std::chrono::system_clock::time_point _deadline;
    bool _timed_out;
    bool _wd_alive;
    bool _wd_active;
};

class JSCxAutoTimeout {
  public:
    explicit JSCxAutoTimeout(JSCx* cx, int ms_timeout);
    ~JSCxAutoTimeout();

  private:
    JSCx* _cx;
};

#endif  // included js.h