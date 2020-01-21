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

#include <list>

#include "common.h"
#include "js_mgr.h"
#include "message.h"

class Connection : public std::enable_shared_from_this<Connection> {
  public:
    typedef std::shared_ptr<Connection> Ptr;
    typedef http::request<http::string_body> Request;
    typedef http::response<http::string_body> Response;
    typedef std::shared_ptr<Response> RespPtr;

    Connection(tcp::socket sock, JSManager& js_mgr);
    ~Connection();

    beast::tcp_stream::socket_type& socket();

    void start();

    void respond(http::status code, const char* message);
    void respond(http::status code, std::string& body);

  private:
    void handle_request();

    void do_handshake();
    void do_read();
    void do_write(RespPtr resp);
    void do_close();

    beast::tcp_stream _stream;
    beast::flat_buffer _buffer;
    Request _req;

    JSManager& _js_mgr;
};

#endif  // Included connection.h
