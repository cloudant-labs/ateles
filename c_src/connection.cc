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

#include <errno.h>
#include <stdio.h>

#include "stats.h"
#include "util.h"

#define TRACE fprintf(stderr, "%s:%d\n", __FUNCTION__, __LINE__);

Connection::Connection(tcp::socket sock, JSManager& js_mgr) :
    _stream(std::move(sock)), _js_mgr(js_mgr)
{
    ATELES_STAT_OPEN_CONNS++;
}

Connection::~Connection()
{
    ATELES_STAT_OPEN_CONNS--;
}

beast::tcp_stream::socket_type&
Connection::socket()
{
    return _stream.socket();
}

void
Connection::start()
{
    auto self(shared_from_this());
    asio::dispatch(_stream.get_executor(), [this, self]() { do_read(); });
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

    switch(code) {
        case http::status::ok:
            ATELES_STAT_HTTP_200++;
            break;
        case http::status::bad_request:
            ATELES_STAT_HTTP_400++;
            break;
        case http::status::not_found:
            ATELES_STAT_HTTP_404++;
            break;
        case http::status::method_not_allowed:
            ATELES_STAT_HTTP_405++;
            break;
        default:
            ATELES_STAT_HTTP_UNK++;
            break;
    }

    RespPtr resp = std::make_shared<Response>(code, _req.version());
    resp->set(http::field::server, BOOST_BEAST_VERSION_STRING);
    resp->set(http::field::content_type, ctype);
    resp->keep_alive(_req.keep_alive());
    resp->body() = body;
    resp->prepare_payload();

    auto self(shared_from_this());
    asio::dispatch(
        _stream.get_executor(), [this, self, resp]() { do_write(resp); });
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
                report_error("conn_read", ec);
                return;
            }

            if(_req.method() == http::verb::get && _req.target() == "/Health") {
                respond(http::status::ok, "OK");
                return;
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
                report_error("conn_write", ec);
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

    beast::error_code ec;
    socket().shutdown(tcp::socket::shutdown_send, ec);

    // Ignore closing a disconnected socket
    if(ec && ec.value() != ENOTCONN) {
        report_error("conn_close", ec);
    }
}
