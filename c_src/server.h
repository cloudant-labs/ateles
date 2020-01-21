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

#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/core/noncopyable.hpp>
#include <set>

#include "connection_mgr.h"
#include "js_mgr.h"

namespace asio = boost::asio;
namespace bsys = boost::system;

class ServerOpts {
public:
    std::string address;
    std::string port;
    std::string cert;
    std::string key;
    size_t max_mem;
};


class Server : private boost::noncopyable {
  public:
    Server(ServerOpts& opts);

    void run();

  private:
    void do_accept();
    void do_await_stop();

    asio::io_context _io_ctx;
    asio::ssl::context _ssl_ctx;
    asio::signal_set _signals;
    asio::ip::tcp::acceptor _acceptor;

    ConnectionManager _conn_mgr;
    JSManager _js_mgr;
};

#endif  // Included server.h