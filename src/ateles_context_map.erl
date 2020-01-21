% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(ateles_context_map).
-behavior(ateles_context).


-export([
    start_link/3,
    stop/1,

    map_doc_async/2,
    map_doc_recv/1
]).


-export([
    init/4
]).


start_link(Stream, JSCtxId, {Lib, MapFuns}) ->
    proc_lib:start_link(?MODULE, init, [Stream, JSCtxId, Lib, MapFuns]).


stop(Pid) ->
    {ok, Resp} = gen:call(Pid, call, stop),
    Resp.


map_doc_async({_CtxId, Pid}, Doc) ->
    ateles_util:gen_call_async(Pid, {map_doc, Doc}).


map_doc_recv(Ref) ->
    {ok, Resp} = ateles_util:gen_recv_async(Ref),
    Resp.


init(Conn, JSCtxId, Lib, MapFuns0) ->
    proc_lib:init_ack({ok, self()}),
    {ok, Stream} = ateles_conn_server:get_stream(Conn),

    {ok, MapFuns1} = ateles:rewrite(MapFuns0),

    {ok, _} = ateles_util:create_ctx(Stream, JSCtxId),
    {ok, _} = ateles_util:eval_file(Stream, JSCtxId, "map.js"),
    {ok, true} = ateles_util:call(Stream, JSCtxId, <<"init">>, [Lib, MapFuns1]),

    loop(Stream, JSCtxId, queue:new()).


loop(Stream, JSCtxId, Queue) ->
    receive
        {call, From, {map_doc, Doc}} ->
            ok = ateles_util:call_async(Stream, JSCtxId, <<"mapDoc">>, [Doc]),
            loop(Stream, JSCtxId, queue:in(From, Queue));
        {call, From, stop} ->
            drain_queue(Queue, closing),
            gen:reply(From, ok),
            ateles_util:destroy_ctx(Stream, JSCtxId);
        Msg ->
            case ateles_util:handle_async_resp(Stream, Msg) of
                {exit, Reason} ->
                    exit(Reason);
                Resp ->
                    {{value, From}, RestQueue} = queue:out(Queue),
                    gen:reply(From, Resp),
                    loop(Stream, JSCtxId, RestQueue)
            end
    end.


drain_queue(Queue, Msg) ->
    case queue:out(Queue) of
        {{value, From}, RestQueue} ->
            gen:reply(From, Msg),
            drain_queue(RestQueue, Msg);
        {empty, _} ->
            ok
    end.
