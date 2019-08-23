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

#ifndef ATELES_EXCEPTIONS_H
#define ATELES_EXCEPTIONS_H

#include <exception>

#include <grpc/grpc.h>
#include <grpcpp/security/server_credentials.h>
#include <grpcpp/server.h>
#include <grpcpp/server_builder.h>
#include <grpcpp/server_context.h>

namespace ateles
{
class AtelesExit : public std::exception {
};

class AtelesError : public std::exception {
  public:
    explicit AtelesError(const std::string& what) : _what(what) {}

    virtual ~AtelesError() throw() {}

    virtual grpc::StatusCode code() const throw() = 0;

    virtual const char* what() const throw() { return this->_what.c_str(); }

  protected:
    std::string _what;
};

class AtelesInvalidArgumentError : public AtelesError {
  public:
    explicit AtelesInvalidArgumentError(const std::string& what)
        : AtelesError(what)
    {
    }

    virtual grpc::StatusCode code() const throw()
    {
        return grpc::StatusCode::INVALID_ARGUMENT;
    }
};

class AtelesNotFoundError : public AtelesError {
  public:
    explicit AtelesNotFoundError(const std::string& what) : AtelesError(what) {}

    virtual grpc::StatusCode code() const throw()
    {
        return grpc::StatusCode::NOT_FOUND;
    }
};

class AtelesResourceExhaustedError : public AtelesError {
  public:
    explicit AtelesResourceExhaustedError(const std::string& what)
        : AtelesError(what)
    {
    }

    virtual grpc::StatusCode code() const throw()
    {
        return grpc::StatusCode::RESOURCE_EXHAUSTED;
    }
};

class AtelesInternalError : public AtelesError {
  public:
    explicit AtelesInternalError(const std::string& what) : AtelesError(what) {}

    virtual grpc::StatusCode code() const throw()
    {
        return grpc::StatusCode::INTERNAL;
    }
};

}  // namespace ateles

#endif  // included exceptions.h