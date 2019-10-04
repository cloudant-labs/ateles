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
    start_link/1,
    stop/1,

    map_doc_async/2,
    map_doc_recv/1
]).


-export([
    init/2
]).


start_link({Lib, MapFuns}) ->
    proc_lib:start_link(?MODULE, init, [Lib, MapFuns]).


stop(Pid) ->
    {ok, Resp} = gen:call(Pid, call, stop),
    Resp.


map_doc_async({_CtxId, Pid}, Doc) ->
    ateles_util:gen_call_async(Pid, {map_doc, Doc}).


map_doc_recv(Ref) ->
    {ok, Resp} = ateles_util:gen_recv_async(Ref),
    Resp.


init(Lib, MapFuns0) ->
    proc_lib:init_ack({ok, self()}),
    {ok, MapFuns1} = ateles:rewrite(MapFuns0),
    Args = [Lib, MapFuns1],
    loop(new_stream(Args), Args, queue:new()).


loop(Stream, Args, Queue) ->
    receive
        {call, From, {map_doc, Doc}} ->
            ok = ateles_util:call_async(Stream, <<"mapDoc">>, [Doc]),
            loop(Stream, Args, queue:in(From, Queue));
        {call, From, stop} ->
            drain_queue(Queue, closing),
            gen:reply(From, ok),
            grpcbox_client:close_and_recv(Stream);
        GrpcMsg ->
            case ateles_util:handle_async_resp(Stream, GrpcMsg) of
                skip ->
                    loop(Stream, Args, Queue);
                stream_finished ->
                    drain_queue(Queue, retry),
                    loop(new_stream(Args), Args, queue:new());
                {exit, Reason} ->
                    exit(Reason);
                Resp ->
                    {{value, From}, RestQueue} = queue:out(Queue),
                    gen:reply(From, Resp),
                    loop(Stream, Args, RestQueue)
            end
    end.


new_stream(Args) ->
    {ok, Stream} = ateles_client:execute(#{channel => ateles}),
    {ok, _} = ateles_util:eval_file(Stream, "map.js"),
    {ok, true} = ateles_util:call(Stream, <<"init">>, Args),
    Stream.


drain_queue(Queue, Msg) ->
    case queue:out(Queue) of
        {{value, From}, RestQueue} ->
            gen:reply(From, Msg),
            drain_queue(RestQueue, Msg);
        {empty, _} ->
            ok
    end.
