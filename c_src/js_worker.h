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

#ifndef ATELES_JS_WORKER_H
#define ATELES_JS_WORKER_H

#include <boost/thread.hpp>
#include <queue>

#include "js.h"
#include "message.h"

class JSWorker {
  public:
    typedef std::shared_ptr<JSWorker> Ptr;

    JSWorker(size_t max_mem, bool rewriter = false);

    void stop();

    void submit(Message::Ptr message);

  private:
    void run(size_t max_mem, bool rewriter);
    bool run_one(JSCx* jscx);
    Message::Ptr get_next_message();

    std::unique_ptr<boost::thread> _thread;
    std::mutex _lock;
    std::condition_variable _cv;

    bool _running;
    std::queue<Message::Ptr> _messages;
};

#endif  // Included js_mgr.h
