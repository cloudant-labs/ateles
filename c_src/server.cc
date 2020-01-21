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

#include "server.h"

#include "connection.h"

Server::Server(ServerOpts& opts) :
    _io_ctx(),
    _ssl_ctx(asio::ssl::context::sslv23),
    _signals(_io_ctx),
    _acceptor(_io_ctx),
    _conn_mgr(),
    _js_mgr(opts.max_mem)
{
    _ssl_ctx.set_options(asio::ssl::context::default_workarounds
        | asio::ssl::context::no_sslv2 | asio::ssl::context::single_dh_use);
    _ssl_ctx.use_certificate_chain_file(opts.cert);
    _ssl_ctx.use_private_key_file(opts.key, asio::ssl::context::pem);

    _signals.add(SIGINT);
    _signals.add(SIGTERM);

    do_await_stop();

    asio::ip::tcp::resolver r(_io_ctx);
    asio::ip::tcp::endpoint ep = *(r.resolve(opts.address, opts.port)).begin();

    _acceptor.open(ep.protocol());
    _acceptor.set_option(asio::ip::tcp::acceptor::reuse_address(true));
    _acceptor.bind(ep);
    _acceptor.listen();

    do_accept();
}

void
Server::run()
{
    _io_ctx.run();
}

void
Server::do_accept()
{
    _acceptor.async_accept([this](bsys::error_code error, tcp_socket sock) {
        if(!_acceptor.is_open()) {
            return;
        }

        if(error) {
            do_accept();
            return;
        }

        auto conn =
            std::make_shared<Connection>(_io_ctx, _ssl_ctx, std::move(sock), _conn_mgr, _js_mgr);
        _conn_mgr.start(conn);

        do_accept();
    });
}

void
Server::do_await_stop()
{
    _signals.async_wait([this](bsys::error_code error, int signo) {
        _acceptor.close();
        _conn_mgr.stop_all();
    });
}
