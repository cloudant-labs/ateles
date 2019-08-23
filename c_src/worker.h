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

#ifndef ATELES_WORKER_H
#define ATELES_WORKER_H

#include <functional>
#include <future>
#include <queue>
#include <string>
#include <vector>

#include "errors.h"
#include "js.h"

namespace ateles
{
class Task {
  public:
    typedef std::function<std::string(Context*)> TaskCallBack;
    typedef std::unique_ptr<Task> Ptr;

    explicit Task(TaskCallBack cb) : _cb(cb), _promise() {}

    static Ptr create(TaskCallBack cb) { return std::make_unique<Task>(cb); }

    bool operator()(Context* cx)
    {
        try {
            this->_promise.set_value(this->_cb(cx));
            return true;
        } catch(AtelesExit&) {
            this->_promise.set_value_at_thread_exit("");
            return false;
        } catch(...) {
            this->_promise.set_exception(std::current_exception());
            return true;
        }
    }

    JSFuture get_future() { return this->_promise.get_future(); }

  private:
    TaskCallBack _cb;
    JSPromise _promise;
};

class Worker {
  public:
    typedef std::shared_ptr<Worker> Ptr;
    typedef std::pair<std::string, std::string> Result;

    explicit Worker();
    ~Worker();

    static Ptr create()
    {
        Ptr ret = std::make_unique<Worker>();
        ret->_inited.wait();
        if(ret->_inited.get()) {
            return ret;
        }
        return Ptr();
    }

    void exit();

    JSFuture set_lib(const std::string& lib);
    JSFuture add_map_fun(const std::string& id, const std::string& source);
    JSFuture map_doc(const std::string& doc);

  private:
    void run(std::promise<bool> inited);

    JSFuture add_task(Task::TaskCallBack cb);
    bool get_task(Task::Ptr& task);

    std::unique_ptr<std::thread> _thread;

    std::mutex _task_lock;
    std::condition_variable _task_cv;
    std::queue<Task::Ptr> _tasks;
    std::future<bool> _inited;
};

}  // namespace ateles

#endif  // included worker.h