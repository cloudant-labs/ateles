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

#ifndef ATELES_SERVER_H
#define ATELES_SERVER_H

#include <memory>
#include <set>

#include "common.h"
#include "js_mgr.h"

class ServerOpts {
  public:
    std::string address;
    std::string port;
    size_t num_threads;
    size_t max_mem;
};

class Listener : public std::enable_shared_from_this<Listener> {
  public:
    Listener(ServerOpts& opts, asio::io_context& io_ctx, JSManager& js_mgr);
    void close();
    void run();

  private:
    void do_accept();

    asio::io_context& _io_ctx;
    tcp::acceptor _acceptor;

    JSManager& _js_mgr;
};

class Server : private boost::noncopyable {
  public:
    Server(ServerOpts& opts);

    void run();

  private:
    asio::io_context _io_ctx;

    size_t _num_threads;
    std::vector<std::thread> _io_threads;

    JSManager _js_mgr;
    Listener _listener;
};

#endif  // Included server.h