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

#include "message.h"

#include "connection.h"

Message::Message(Connection::Ptr conn, JSRequest req) :
    _conn(conn), _req(req), _resp()
{
}

Message::Ptr
Message::create(std::shared_ptr<Connection> conn, std::string& mesg)
{
    JSRequest req;
    if(!req.ParseFromString(mesg)) {
        return Ptr();
    }

    return std::make_shared<Message>(conn, req);
}

JSRequest*
Message::get_request()
{
    return &_req;
}

void
Message::set_response(int32_t status, std::string result)
{
    _resp.set_status(status);
    _resp.set_result(result);
}

void
Message::send()
{
    http::status code = http::status::ok;
    std::string body;
    if(!_resp.SerializeToString(&body)) {
        code = http::status::internal_server_error;
        body = "Protobuf serialization failed.";
    }

    _conn->respond(code, body);
}
