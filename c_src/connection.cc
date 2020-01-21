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

#include "connection.h"

#include <stdio.h>

#define TRACE fprintf(stderr, "%s:%d\n", __FUNCTION__, __LINE__);

Connection::Connection(asio::ssl::context& ssl_ctx,
    tcp::socket sock,
    ConnectionManager& conn_mgr,
    JSManager& js_mgr) :
    _stream(std::move(sock), ssl_ctx), _conn_mgr(conn_mgr), _js_mgr(js_mgr)
{
}

Connection::~Connection() {
}

beast::tcp_stream::socket_type&
Connection::socket()
{
    return beast::get_lowest_layer(_stream).socket();
}

void
Connection::start()
{
    auto self(shared_from_this());
    asio::dispatch(_stream.get_executor(), [this, self]() { do_handshake(); });
}

void
Connection::respond(http::status code, const char* message)
{
    std::string body(message);
    respond(code, body);
}

void
Connection::respond(http::status code, std::string& body)
{
    std::string ctype;
    if(code == http::status::ok) {
        ctype = "application/octet-stream";
    } else {
        ctype = "text/plain";
    }

    RespPtr resp = std::make_shared<Response>(code, _req.version());
    resp->set(http::field::server, BOOST_BEAST_VERSION_STRING);
    resp->set(http::field::content_type, ctype);
    resp->keep_alive(_req.keep_alive());
    resp->body() = body;
    resp->prepare_payload();

    auto self(shared_from_this());
    asio::dispatch(_stream.get_executor(), [this, self, resp]() {
        do_write(resp);
    });
}

void
Connection::do_handshake()
{
    if(!socket().is_open()) {
        return;
    }

    auto self(shared_from_this());
    _stream.async_handshake(
        ssl::stream_base::server, [this, self](const beast::error_code& ec) {
            if(!ec) {
                do_read();
            }
        });
}

void
Connection::do_read()
{
    if(!socket().is_open()) {
        return;
    }

    // Reset our request object
    _req = {};

    beast::get_lowest_layer(_stream).expires_after(std::chrono::seconds(30));

    auto self(shared_from_this());
    http::async_read(_stream,
        _buffer,
        _req,
        [this, self](beast::error_code ec, size_t nread) {
            boost::ignore_unused(nread);

            if(ec == http::error::end_of_stream) {
                return do_close();
            } else if(ec) {
                _conn_mgr.stop(self);
            }

            if(_req.target() != "/Ateles/Execute") {
                respond(http::status::not_found, "Path not found.");
                return;
            }

            if(_req.method() != http::verb::post) {
                respond(http::status::method_not_allowed, "Allowed: POST");
            }

            Message::Ptr mesg = Message::create(self, _req.body());
            if(!mesg) {
                respond(http::status::bad_request, "Invalid protobuf message.");
                return;
            }

            _js_mgr.submit(mesg);
        });
}

void
Connection::do_write(Connection::RespPtr resp)
{
    if(!socket().is_open()) {
        return;
    }

    auto self(shared_from_this());
    http::async_write(_stream,
        *resp,
        [this, self, resp](beast::error_code ec, size_t nwritten) {
            boost::ignore_unused(nwritten);

            if(ec) {
                return;
            }

            if(resp->need_eof()) {
                return do_close();
            }

            do_read();
        });
}

void
Connection::do_close()
{
    if(!socket().is_open()) {
        return;
    }

    beast::get_lowest_layer(_stream).expires_after(std::chrono::seconds(30));

    auto self(shared_from_this());
    _stream.async_shutdown([self](beast::error_code ec) {
        boost::ignore_unused(self);
        return;
    });
}
