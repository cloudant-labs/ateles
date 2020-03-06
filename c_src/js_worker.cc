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

#include "js_worker.h"
#include "stats.h"

JSWorker::JSWorker(size_t max_mem) : _running(true)
{
    boost::thread::attributes attrs;
    attrs.set_stack_size(8 * 1024 * 1024);
    this->_thread = std::make_unique<boost::thread>(
        attrs, [this, max_mem] { this->run(max_mem); });
}

void
JSWorker::stop()
{
    std::lock_guard<std::mutex> guard(_lock);
    _running = false;
    _cv.notify_one();
}

void
JSWorker::submit(Message::Ptr message)
{
    std::lock_guard<std::mutex> guard(_lock);
    if(!_running) {
        return;
    }

    _messages.push(message);
    _cv.notify_one();
}

void
JSWorker::run(size_t max_mem)
{
    JSCx::Ptr jscx = std::make_unique<JSCx>(max_mem);
    bool go = run_one(jscx.get());
    while(go) {
        go = run_one(jscx.get());
    }
}

bool
JSWorker::run_one(JSCx* jscx)
{
    auto msg = get_next_message();

    // An empty message means we're done working
    if(!msg) {
        return false;
    }

    auto req = msg->get_request();
    std::vector<std::string> args;
    for(int i = 0; i < req->args_size(); i++) {
        args.push_back(req->args(i));
    }

    try {
        std::string resp;

        int timeout = req->timeout();
        if(timeout <= 0) {
            timeout = 5000;
        }
        JSCxAutoTimeout auto_to(jscx, timeout);

        if(req->action() == JSRequest::EVAL) {
            resp = jscx->eval(req->script(), args);
        } else if(req->action() == JSRequest::CALL) {
            resp = jscx->call(req->script(), args);
        }
        ATELES_STAT_JS_SUCCESS++;
        msg->set_response(0, resp);
    } catch(AtelesError& error) {
        ATELES_STAT_JS_ERROR++;
        msg->set_response(1, error.what());
    }

    msg->send();

    return true;
}

Message::Ptr
JSWorker::get_next_message()
{
    std::unique_lock<std::mutex> lock(_lock);
    _cv.wait(lock, [this] { return !_running || _messages.size() > 0; });

    if(!_running) {
        return Message::Ptr();
    }

    auto ret = _messages.front();
    _messages.pop();
    return ret;
}
