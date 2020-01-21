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

#define TRACE(msg) fprintf(stderr, "%s:%d - %s\n", __FUNCTION__, __LINE__, msg)

Connection::Connection(
    asio::io_context& io_ctx,
    asio::ssl::context& ssl_ctx,
    tcp_socket sock,
    ConnectionManager& conn_mgr,
    JSManager& js_mgr) :
    _io_ctx(io_ctx), _socket(std::move(sock), ssl_ctx), _conn_mgr(conn_mgr), _js_mgr(js_mgr)
{
}

Connection::~Connection()
{
}

ssl_socket::lowest_layer_type&
Connection::socket()
{
    return _socket.lowest_layer();
}

void
Connection::start()
{
    do_handshake();
}

void
Connection::stop()
{
    socket().close();
}

void
Connection::reply(Message::Ptr msg)
{
    asio::post(_io_ctx, [this, msg] {
        if(!socket().is_open()) {
            return;
        }

        _write_queue.push_back(msg);
        if(_write_queue.size() == 1) {
            do_write();
        }
    });
}

void
Connection::do_handshake()
{
    if(!socket().is_open()) {
        return;
    }

    auto self(shared_from_this());
    _socket.async_handshake(asio::ssl::stream_base::server,
        [this, self](const bsys::error_code& error) {
            if(!error) {
                do_read_header();
            }
        });
}

void
Connection::do_read_header()
{
    if(!socket().is_open()) {
        return;
    }

    auto self(shared_from_this());
    _buffer.resize(Message::HeaderLength);

    asio::async_read(_socket,
        asio::buffer(_buffer),
        [this, self](bsys::error_code error, size_t nread) {
            if(error == asio::error::operation_aborted) {
                return;
            } else if(error) {
                _conn_mgr.stop(shared_from_this());
                return;
            }

            uint32_t msg_len = 0;
            uint32_t stream_id = 0;
            uint8_t flags = 0;
            if(!Message::decode_header(_buffer, msg_len, stream_id, flags)) {
                _conn_mgr.stop(shared_from_this());
                return;
            }

            do_read_body(msg_len, stream_id, flags);
        });
}

void
Connection::do_read_body(uint32_t msg_len, uint32_t stream_id, uint8_t flags)
{
    if(!socket().is_open()) {
        return;
    }

    auto self(shared_from_this());
    _buffer.resize(msg_len - 5);

    asio::async_read(_socket,
        asio::buffer(_buffer),
        [this, self, stream_id, flags](bsys::error_code error, size_t nread) {
            if(error == asio::error::operation_aborted) {
                return;
            } else if(error) {
                _conn_mgr.stop(shared_from_this());
                return;
            }

            // Message start
            if((flags & 1) == 1) {
                if(_messages.find(stream_id) != _messages.end()) {
                    _conn_mgr.stop(shared_from_this());
                    return;
                }

                auto msg = std::make_shared<Message>(shared_from_this(), stream_id);
                _messages[stream_id] = msg;
            }

            _messages[stream_id]->append(_buffer);

            if((flags & 2) == 2) {
                auto iter = _messages.find(stream_id);
                if(iter == _messages.end()) {
                    _conn_mgr.stop(shared_from_this());
                    return;
                }

                auto msg = (*iter).second;
                _messages.erase(iter);

                // TODO: Actually handle the message
                _js_mgr.submit(msg);
            }

            do_read_header();
        });
}

void
Connection::do_write()
{
    if(!socket().is_open()) {
        return;
    }

    auto self(shared_from_this());
    auto msg = _write_queue.front();
    _write_queue.pop_front();
    auto fragment = msg->dequeue_fragment();
    if(!msg->empty()) {
        _write_queue.push_back(msg);
    }

    asio::async_write(_socket,
        asio::buffer(fragment),
        [this, self](bsys::error_code error, size_t nwritten) {
            if(error == asio::error::operation_aborted) {
                return;
            } else if(error) {
                _conn_mgr.stop(shared_from_this());
                return;
            }
            if(_write_queue.size() > 0) {
                do_write();
            }
        });
}
