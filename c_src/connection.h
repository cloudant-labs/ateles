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

#ifndef ATELES_CONNECTION_H
#define ATELES_CONNECTION_H

#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>
#include <list>
#include <memory>

#include "connection_mgr.h"
#include "js_mgr.h"
#include "message.h"

namespace asio = boost::asio;
namespace bsys = boost::system;

typedef asio::ip::tcp::socket tcp_socket;
typedef asio::ssl::stream<tcp_socket> ssl_socket;

class Connection : public std::enable_shared_from_this<Connection> {
  public:
    typedef std::shared_ptr<Connection> Ptr;

    Connection(asio::io_context& io_ctx,
        asio::ssl::context& ssl_ctx,
        tcp_socket sock,
        ConnectionManager& conn_mgr,
        JSManager& js_mgr);
    ~Connection();

    ssl_socket::lowest_layer_type& socket();

    void start();
    void stop();

    void reply(Message::Ptr msg);

  private:
    void do_handshake();
    void do_read_header();
    void do_read_body(uint32_t msg_len, uint32_t stream_id, uint8_t flags);
    void do_write();

    asio::io_context& _io_ctx;
    ssl_socket _socket;

    ConnectionManager& _conn_mgr;
    JSManager& _js_mgr;

    std::map<uint32_t, Message::Ptr> _messages;
    std::list<Message::Ptr> _write_queue;
    std::vector<uint8_t> _buffer;
};

#endif  // Included connection.h
