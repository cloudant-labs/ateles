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

Listener::Listener(ServerOpts& opts,
    asio::io_context& io_ctx,
    ssl::context& ssl_ctx,
    ConnectionManager& conn_mgr,
    JSManager& js_mgr) :
    _io_ctx(io_ctx),
    _ssl_ctx(ssl_ctx),
    _acceptor(io_ctx),
    _conn_mgr(conn_mgr),
    _js_mgr(js_mgr)
{
    asio::ip::tcp::resolver r(_io_ctx);
    asio::ip::tcp::endpoint ep = *(r.resolve(opts.address, opts.port)).begin();

    _acceptor.open(ep.protocol());
    _acceptor.set_option(tcp::acceptor::reuse_address(true));
    _acceptor.bind(ep);
    _acceptor.listen(asio::socket_base::max_listen_connections);
}

void
Listener::close()
{
    _acceptor.close();
}

void
Listener::run()
{
    do_accept();
}

void
Listener::do_accept()
{
    _acceptor.async_accept(asio::make_strand(_io_ctx),
        [this](bsys::error_code error, tcp::socket sock) {
            if(!_acceptor.is_open()) {
                return;
            }

            if(error) {
                do_accept();
                return;
            }

            std::make_shared<Connection>(
                _ssl_ctx, std::move(sock), _conn_mgr, _js_mgr)->start();

            do_accept();
        });
}

Server::Server(ServerOpts& opts) :
    _io_ctx(opts.num_threads),
    _ssl_ctx(asio::ssl::context::sslv23),
    _signals(_io_ctx),
    _num_threads(opts.num_threads),
    _conn_mgr(),
    _js_mgr(opts.max_mem),
    _listener(opts, _io_ctx, _ssl_ctx, _conn_mgr, _js_mgr)
{
    _ssl_ctx.set_options(asio::ssl::context::default_workarounds
        | asio::ssl::context::no_sslv2 | asio::ssl::context::single_dh_use);
    _ssl_ctx.use_certificate_chain_file(opts.cert);
    _ssl_ctx.use_private_key_file(opts.key, asio::ssl::context::pem);

    _signals.add(SIGINT);
    _signals.add(SIGTERM);

    _num_threads = std::max<size_t>(1, _num_threads);

    do_await_stop();
    _listener.run();
}

void
Server::run()
{
    _listener.run();

    _io_threads.reserve(_num_threads - 1);
    for(auto i = _num_threads - 1; i > 0; --i) {
        _io_threads.emplace_back([this] { _io_ctx.run(); });
    }
    _io_ctx.run();
}

void
Server::do_await_stop()
{
    _signals.async_wait([this](bsys::error_code error, int signo) {
        _listener.close();
        _conn_mgr.stop_all();
    });
}
