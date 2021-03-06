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
#include "stats.h"
#include "util.h"

Listener::Listener(ServerOpts& opts,
    asio::io_context& io_ctx,
    JSWorker::Ptr rewriter) :
    _io_ctx(io_ctx), _acceptor(io_ctx), _rewriter(rewriter)
{
    asio::ip::tcp::resolver r(_io_ctx);
    asio::ip::tcp::endpoint ep = *(r.resolve(opts.address, opts.port)).begin();

    _acceptor.open(ep.protocol());
    _acceptor.set_option(tcp::acceptor::reuse_address(true));
    _acceptor.bind(ep);
    _acceptor.listen(-1);

    _max_mem = opts.max_mem;
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
                report_error("accept", error);
                do_accept();
                return;
            }

            ATELES_STAT_ACCEPTS++;

            std::make_shared<Connection>(std::move(sock), _rewriter, _max_mem)
                ->start();

            do_accept();
        });
}

Server::Server(ServerOpts& opts) :
    _io_ctx(opts.num_threads),
    _num_threads(opts.num_threads),
    _rewriter(std::make_shared<JSWorker>(opts.max_mem, true)),
    _listener(opts, _io_ctx, _rewriter)
{
    _num_threads = std::max<size_t>(1, _num_threads);
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
