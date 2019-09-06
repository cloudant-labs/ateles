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
#include <atomic>
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
#include "errors.h"
#include "js.h"


namespace ateles
{


class Worker;
class Task;


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
        grpc::ServerCompletionQueue* queue,
        JSCx* vm);
    ~Connection();

    void submit();

    // State handlers
    void handle_connected();
    void handle_request();
    void handle_response();
    void handle_finished();
    void handle_disconnected();

    void check_tid();

  private:
    Task* _mktask(std::function<void(Ptr)> func);

    std::thread::id _tid;

    Ateles::AsyncService* _service;
    grpc::ServerCompletionQueue* _queue;
    grpc::ServerContext _context;
    grpc::ServerAsyncReaderWriter<JSResponse, JSRequest> _stream;

    JSRequest _req;
    JSResponse _resp;

    JSCx* _cx;
    JSCompartment::Ptr _compartment;
};

class Task {
  public:
    typedef std::function<void(Connection::Ptr)> TaskCallBack;

    explicit Task(Connection::Ptr conn, TaskCallBack cb) : _conn(conn), _cb(cb) {
    }

    void run() {
        this->_conn->check_tid();
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

    JSCx::Ptr cx = std::make_unique<JSCx>();

    Connection::Ptr conn =
        std::make_shared<Connection>(this->_service, this->_queue.get(), cx.get());
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
    grpc::ServerCompletionQueue* queue, JSCx* cx)
    : _tid(std::this_thread::get_id()), _service(service), _queue(queue), _stream(&_context), _cx(cx)
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
    this->check_tid();
}

void
Connection::submit()
{
    Task* t = this->_mktask([](Ptr p) { p->handle_connected(); });
    this->_service->RequestExecute(&_context, &_stream, _queue, _queue, t);
}

void
Connection::handle_connected()
{
    // Once we're in a connected state we have to
    // add a new empty connection to wait for the next
    // gRPC stream to connect. This is beyond odd.
    Ptr new_conn = std::make_shared<Connection>(this->_service, this->_queue, this->_cx);
    new_conn->submit();

    try {
        this->_compartment = this->_cx->new_compartment();
    } catch(AtelesError& err) {
        Task* t = this->_mktask([](Ptr p) { p->handle_disconnected(); });
        grpc::Status status(err.code(), err.what());
        this->_stream.Finish(status, t);
        return;
    }

    Task* t = this->_mktask([](Ptr p) { p->handle_request(); });
    this->_stream.Read(&this->_req, t);
}


void
Connection::handle_request()
{
    this->_resp.Clear();
    this->_resp.set_status(0);

    std::vector<std::string> args;
    for(int i = 0; i < this->_req.args_size(); i++) {
        args.push_back(this->_req.args(i));
    }

    try {
        std::string result;
        if(this->_req.action() == JSRequest_Action_EVAL) {
            result = this->_compartment->eval(this->_req.script(), args);
        } else {
            result = this->_compartment->call(this->_req.script(), args);
        }
        this->_resp.set_result(result);
    } catch(AtelesError& err) {
        this->_resp.set_status(err.code());
        this->_resp.set_result(err.what());
    }

    Task* t = this->_mktask([](Ptr p) { p->handle_response(); });
    this->_stream.Write(this->_resp, t);
}


void
Connection::handle_response()
{
    Task* t = this->_mktask([](Ptr p) { p->handle_request(); });
    this->_stream.Read(&this->_req, t);
}


void
Connection::handle_finished()
{
    if(this->_context.IsCancelled()) {
        return;
    }

    Task* t = this->_mktask([](Ptr p) { p->handle_disconnected(); });
    this->_stream.Finish(grpc::Status::OK, t);
}

void
Connection::handle_disconnected()
{
}


void
Connection::check_tid()
{
    if(std::this_thread::get_id() != this->_tid) {
        exit(127);
    }
}


Task*
Connection::_mktask(std::function<void(Ptr)> func)
{
    Ptr tptr = this->shared_from_this();
    return new Task(tptr, func);
}

}  // namespace ateles


void
exit_cleanly(int signum)
{
    exit(1);
}


int
main(int argc, const char* argv[])
{
    std::signal(SIGINT, exit_cleanly);

    // Docs say we have to create at least one JSContext
    // in a single threaded manner. So here we are.
    JS_Init();
    JS_NewContext(8L * 1024 * 1024);

    ateles::Server server;
    server.start(5);
    server.wait();

    exit(0);
}