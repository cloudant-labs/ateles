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

#include "js_mgr.h"

JSManager::JSManager(size_t max_mem) : _workers(), _max_mem(max_mem) {}

std::list<std::string>
JSManager::list()
{
    std::list<std::string> ret;

    for(const auto& kv : _workers) {
        ret.push_back(kv.first);
    }

    return ret;
}

void
JSManager::submit(Message::Ptr message)
{
    auto req = message->get_request();
    auto ctx_id = req->context_id();
    if(req->action() == JSRequest::CREATE_CTX) {
        if(_workers.find(ctx_id) != _workers.end()) {
            message->set_response(1, "context_id exists");
        } else {
            _workers[ctx_id] = std::make_shared<JSWorker>(_max_mem);
            message->set_response(0, "true");
        }
        message->reply();
    } else if(req->action() == JSRequest::DESTROY_CTX) {
        auto iter = _workers.find(ctx_id);
        if(iter == _workers.end()) {
            message->set_response(1, "context_id not found");
        } else {
            iter->second->stop();
            message->set_response(0, "true");
        }
        message->reply();
    } else {  // EVAL or CALL
        auto iter = _workers.find(ctx_id);
        if(iter == _workers.end()) {
            message->set_response(1, "context_it not found");
            message->reply();
        } else {
            iter->second->submit(message);
        }
    }
}
