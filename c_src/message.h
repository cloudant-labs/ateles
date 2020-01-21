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

    Message(std::shared_ptr<Connection> conn, uint32_t message_id);

    static bool decode_header(std::vector<uint8_t> header,
        uint32_t& msg_len,
        uint32_t& stream_id,
        uint8_t& flags);

    JSRequest* get_request();
    void set_response(int32_t status, std::string result);
    void reply();

    void append(std::vector<uint8_t> fragment);
    std::vector<uint8_t> dequeue_fragment();
    bool empty();

  private:
    std::shared_ptr<Connection> _conn;
    uint32_t _stream_id;

    JSRequest _req;
    JSResponse _resp;

    bool _req_started;
    bool _resp_started;
    std::vector<uint8_t> _body;
};

#endif  // Included message.h
