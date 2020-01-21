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

#ifndef ATELES_MESSAGE_H
#define ATELES_MESSAGE_H

#include <memory>
#include <vector>

#include "ateles.pb.h"

class Connection;

class Message : public std::enable_shared_from_this<Message> {
  public:
    typedef std::shared_ptr<Message> Ptr;

    enum
    {
        HeaderLength = 7,
        FragmentLength = 65530
    };

    Message(std::shared_ptr<Connection> conn, JSRequest req);

    static Ptr create(std::shared_ptr<Connection> conn, std::string& mesg);

    JSRequest* get_request();
    void set_response(int32_t status, std::string result);
    void send();

  private:
    std::shared_ptr<Connection> _conn;

    JSRequest _req;
    JSResponse _resp;
};

#endif  // Included message.h
