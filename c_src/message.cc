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

Message::Message(Connection::Ptr conn, uint32_t stream_id) :
    _conn(conn),
    _stream_id(stream_id),
    _req(),
    _resp(),
    _req_started(false),
    _resp_started(false)
{
}

bool
Message::decode_header(std::vector<uint8_t> buffer,
    uint32_t& msg_len,
    uint32_t& stream_id,
    uint8_t& flags)
{
    if(buffer.size() != HeaderLength) {
        return false;
    }

    msg_len = 0;
    stream_id = 0;
    flags = 0;

    for(int i = 0; i < 2; i++) {
        msg_len = msg_len * 256 + (static_cast<unsigned>(buffer[i]) & 0xFF);
    }

    for(int i = 2; i < 6; i++) {
        stream_id = stream_id * 256 + (static_cast<unsigned>(buffer[i]) & 0xFF);
    }

    flags = buffer[6];

    return true;
}

JSRequest*
Message::get_request()
{
    if(!_req_started) {
        _req.ParseFromArray(&_body[0], _body.size());
        _req_started = true;
    }

    return &_req;
}

void
Message::set_response(int32_t status, std::string result)
{
    _resp.set_status(status);
    _resp.set_result(result);
}

void
Message::reply()
{
    assert(!_resp_started && "response already started");
    _conn->reply(shared_from_this());
}

void
Message::append(std::vector<uint8_t> fragment)
{
    assert(!_req_started && "message already read");
    _body.insert(_body.end(), fragment.begin(), fragment.end());
}

std::vector<uint8_t>
Message::dequeue_fragment()
{
    uint8_t flags = 0;

    if(!_resp_started) {
        _body.resize(_resp.ByteSizeLong());
        _resp.SerializeToArray(&_body[0], _body.size());
        _resp_started = true;
        flags += 1;
    }

    uint32_t msg_len = 5 + std::min(_body.size(), (size_t) FragmentLength);

    if(_body.size() <= FragmentLength) {
        flags += 2;
    }

    std::vector<uint8_t> ret;
    ret.push_back((msg_len >> 8) & 0xFF);
    ret.push_back((msg_len >> 0) & 0xFF);
    ret.push_back((_stream_id >> 24) & 0xFF);
    ret.push_back((_stream_id >> 16) & 0xFF);
    ret.push_back((_stream_id >> 8) & 0xFF);
    ret.push_back((_stream_id >> 0) & 0xFF);
    ret.push_back(flags);
    ret.insert(ret.end(), _body.begin(), _body.begin() + msg_len - 5);

    _body.erase(_body.begin(), _body.begin() + msg_len - 5);

    return ret;
}

bool
Message::empty()
{
    return _body.size() == 0;
}
