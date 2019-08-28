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

#include <algorithm>
#include <chrono>
#include <cmath>
#include <csignal>
#include <iostream>
#include <memory>
#include <string>

#include <grpc/grpc.h>
#include <grpcpp/security/server_credentials.h>
#include <grpcpp/server.h>
#include <grpcpp/server_builder.h>
#include <grpcpp/server_context.h>

#include "ateles.grpc.pb.h"
#include "ateles_proto.h"

namespace ateles
{
class Server final {
  public:
    ~Server();

    void start();
    void run();

  private:
    std::unique_ptr<grpc::Server> _server;
    std::unique_ptr<grpc::ServerCompletionQueue> _queue;
    Ateles::AsyncService _service;
};

class Connection : public std::enable_shared_from_this<Connection> {
  public:
    typedef std::shared_ptr<Connection> Ptr;

    Connection(Ateles::AsyncService* service,
        grpc::ServerCompletionQueue* queue);
    ~Connection();

    void submit();

    // State handlers
    void handle_connected();
    void handle_received();
    void handle_sent();
    void handle_finished();
    void handle_disconnected();

  private:
    Ateles::AsyncService* _service;
    grpc::ServerCompletionQueue* _queue;
    grpc::ServerContext _context;
    grpc::ServerAsyncReaderWriter<MapDocsResponse, MapDocsRequest> _stream;

    MapDocsRequest _req;
    MapDocsResponse _resp;

    bool _connected;
};

class Task {
  public:
    typedef std::function<void(Connection::Ptr)> TaskCallBack;

    explicit Task(Connection::Ptr conn, TaskCallBack cb) : _conn(conn), _cb(cb) {
    }

    void run() {
        this->_cb(this->_conn);
    }

    void cancel() {
        this->_conn->handle_finished();
    }

    long use_count() {
        return this->_conn.use_count();
    }

  private:
    Connection::Ptr _conn;
    TaskCallBack _cb;
};

Server::~Server()
{
    this->_server->Shutdown();
    this->_queue->Shutdown();
}

void
Server::start()
{
    std::string server_address("0.0.0.0:50051");

    grpc::ServerBuilder builder;
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    builder.RegisterService(&(this->_service));
    this->_queue = builder.AddCompletionQueue();
    this->_server = builder.BuildAndStart();
    std::cout << "Server listening on " << server_address << std::endl;
}

void
Server::run()
{
    Connection::Ptr conn =
        std::make_shared<Connection>(&(this->_service), this->_queue.get());
    conn->submit();
    conn.reset();

    void* tag;
    bool ok;
    while(true) {
        if(!this->_queue->Next(&tag, &ok)) {
            return;
        }

        if(tag == nullptr) {
            continue;
        }

        Task* task = static_cast<Task*>(tag);

        if(ok) {
            task->run();
        } else {
            task->cancel();
        }

        delete task;
    }
}

Connection::Connection(Ateles::AsyncService* service,
    grpc::ServerCompletionQueue* queue)
    : _service(service), _queue(queue), _stream(&_context), _connected(false)
{
    // This is fairly silly. If we don't set this then the
    // call to this->_context.IsCancelled causes a segfault
    // if a client disconnects mid stream. And the only reason
    // we even have to call IsCancelled is because if we don't
    // then this->_stream.Finish will cause a segfault. Le sigh.
    this->_context.AsyncNotifyWhenDone(nullptr);
}

Connection::~Connection()
{
}

void
Connection::submit()
{
    Ptr tptr = this->shared_from_this();
    auto func = [](Ptr p) { p->handle_connected(); };
    Task* t = new Task(tptr, func);
    this->_service->RequestMapDocs(&_context, &_stream, _queue, _queue, t);
}

void
Connection::handle_connected()
{
    Ptr tptr = this->shared_from_this();
    auto func = [](Ptr p) { p->handle_received(); };
    Task* t = new Task(tptr, func);
    this->_stream.Read(&this->_req, t);

    this->_connected = true;

    // fprintf(stderr, "Connected: %s\n", this->_context.peer().c_str());

    // Once we're in a connected state we have to
    // add a new empty connection to wait for the next
    // gRPC stream to connect. This is beyond odd.
    Ptr new_conn = std::make_shared<Connection>(this->_service, this->_queue);
    new_conn->submit();
}

void
Connection::handle_received()
{
    Ptr tptr = this->shared_from_this();
    auto func = [](Ptr p) { p->handle_sent(); };
    Task* t = new Task(tptr, func);

    this->_resp.set_ok(true);
    this->_resp.set_result("ohai there");

    this->_stream.Write(this->_resp, t);
}

void
Connection::handle_sent()
{
    Ptr tptr = this->shared_from_this();
    auto func = [](Ptr p) { p->handle_received(); };
    Task* t = new Task(tptr, func);

    this->_stream.Read(&this->_req, t);
}

void
Connection::handle_finished()
{
    if(!this->_connected) {
        return;
    }

    if(this->_context.IsCancelled()) {
        return;
    }

    Ptr tptr = this->shared_from_this();
    auto func = [](Ptr p) { p->handle_disconnected() ; };
    Task* t = new Task(tptr, func);
    this->_stream.Finish(grpc::Status::OK, t);
}

void
Connection::handle_disconnected()
{
    // fprintf(stderr, "Disconnected: %s\n", this->_context.peer().c_str());
}


}  // namespace ateles

int
main(int argc, const char* argv[])
{
    ateles::Server server;
    server.start();
    server.run();

    exit(0);
}