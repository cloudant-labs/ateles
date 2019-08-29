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
#include <future>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <thread>

#include <grpc/grpc.h>
#include <grpcpp/security/server_credentials.h>
#include <grpcpp/server.h>
#include <grpcpp/server_builder.h>
#include <grpcpp/server_context.h>

#include "ateles.grpc.pb.h"
#include "ateles_proto.h"

namespace ateles
{


class Worker;


class Server final {
  public:
    ~Server();

    void start(size_t num_threads);
    //void run();
    void wait();

  private:
    std::unique_ptr<grpc::Server> _server;
    Ateles::AsyncService _service;
    std::list<std::unique_ptr<Worker>> _workers;
};


class Worker {
  public:
      typedef std::unique_ptr<Worker> Ptr;

      explicit Worker(Ateles::AsyncService* service, std::unique_ptr<grpc::ServerCompletionQueue> queue, std::future<bool> go);

  private:
      void run(std::future<bool> go);

      std::unique_ptr<std::thread> _thread;
      Ateles::AsyncService* _service;
      std::unique_ptr<grpc::ServerCompletionQueue> _queue;
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
    void _check_tid();

    std::thread::id _tid;

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


std::string
get_tid()
{
    std::stringstream ss;
    ss << std::this_thread::get_id();
    return ss.str();
}


Server::~Server()
{
    this->_server->Shutdown();
}


void
Server::start(size_t num_threads)
{
    std::string server_address("0.0.0.0:50051");

    grpc::ServerBuilder builder;
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    builder.RegisterService(&(this->_service));

    std::list<std::promise<bool>> promises;

    for(size_t i = 0; i < num_threads; i++) {
        std::promise<bool> promise;
        auto f = promise.get_future();
        auto cq = builder.AddCompletionQueue();
        auto w = std::make_unique<Worker>(&this->_service, std::move(cq), std::move(f));
        promises.push_back(std::move(promise));
        this->_workers.push_back(std::move(w));
    }

    this->_server = builder.BuildAndStart();
    std::cout << "Server listening on " << server_address << std::endl;

    for(auto & promise : promises) {
        promise.set_value(true);
    }
}

// void
// Server::run()
// {
//
// }

void
Server::wait()
{
    this->_server->Wait();
}

Worker::Worker(Ateles::AsyncService* service, std::unique_ptr<grpc::ServerCompletionQueue> queue, std::future<bool> go)
    : _service(service), _queue(std::move(queue))
{
    this->_thread =
        std::make_unique<std::thread>(&Worker::run, this, std::move(go));
    this->_thread->detach();
}

void
Worker::run(std::future<bool> go)
{
    go.wait();
    if(!go.get()) {
        return;
    }

    fprintf(stderr, "Thread: %s\n", get_tid().c_str());

    Connection::Ptr conn =
        std::make_shared<Connection>(this->_service, this->_queue.get());
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
    : _tid(std::this_thread::get_id()), _service(service), _queue(queue), _stream(&_context), _connected(false)
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
    this->_check_tid();
    Ptr tptr = this->shared_from_this();
    auto func = [](Ptr p) { p->handle_connected(); };
    Task* t = new Task(tptr, func);
    this->_service->RequestMapDocs(&_context, &_stream, _queue, _queue, t);
}

void
Connection::handle_connected()
{
    this->_check_tid();
    Ptr tptr = this->shared_from_this();
    auto func = [](Ptr p) { p->handle_received(); };
    Task* t = new Task(tptr, func);
    this->_stream.Read(&this->_req, t);

    this->_connected = true;

    // Once we're in a connected state we have to
    // add a new empty connection to wait for the next
    // gRPC stream to connect. This is beyond odd.
    Ptr new_conn = std::make_shared<Connection>(this->_service, this->_queue);
    new_conn->submit();
}

void
Connection::handle_received()
{
    this->_check_tid();
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
    this->_check_tid();
    Ptr tptr = this->shared_from_this();
    auto func = [](Ptr p) { p->handle_received(); };
    Task* t = new Task(tptr, func);

    this->_stream.Read(&this->_req, t);
}

void
Connection::handle_finished()
{
    this->_check_tid();
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
    this->_check_tid();
}


void
Connection::_check_tid()
{
    if(std::this_thread::get_id() != this->_tid) {
        exit(127);
    }
}


}  // namespace ateles

int
main(int argc, const char* argv[])
{
    ateles::Server server;
    server.start(5);
    server.wait();

    exit(0);
}