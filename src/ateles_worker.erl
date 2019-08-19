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


-module(ateles_worker).


-export([
    start_link/3,
    stop/1,
    map_start/1,
    map_end/1,
    map_doc/3,
    get_result/2
]).

-export([
    init/3
]).


-ifdef(TEST).
-define(TRACE(Fmt, Args), io:format(standard_error, Fmt ++ "~n", Args)).
-else.
-define(TRACE(Fmt, Args), ok).
-endif.


start_link(CtxId, Lib, MapFuns) ->
    {ok, Pid} = proc_lib:start_link(?MODULE, init, [CtxId, Lib, MapFuns]),
    ?TRACE("~p started ateles_worker ~p", [self(), Pid]),
    {ok, Pid}.


stop(Pid) ->
    ?TRACE("~p stop ~p", [self(), Pid]),
    {ok, Resp} = gen:call(Pid, call, stop),
    Resp.


map_start(Pid) ->
    ?TRACE("~p map_start ~p", [self(), Pid]),
    {ok, Resp} = gen:call(Pid, call, map_start, 1000),
    Resp.


map_end(Pid) ->
    ?TRACE("~p map_end ~p", [self(), Pid]),
    {ok, Resp} = gen:call(Pid, call, map_end),
    Resp.


map_doc(Pid, Id, Doc) ->
    ?TRACE("~p map_doc ~p : ~p ~p", [self(), Pid, Id, Doc]),
    erlang:send(Pid, {map_doc, Id, Doc}),
    ok.


get_result(Pid, Id) ->
    ?TRACE("~p getting result ~p : ~p", [self(), Pid, Id]),
    {ok, Resp} = gen:call(Pid, call, {get_result, Id}),
    Resp.


init(CtxId, Lib, MapFuns) ->
    CreateCtxReq = #{
        context_id => CtxId
    },
    {ok, _, _} = ateles_client:create_context(CreateCtxReq),

    AddMapFunsReq = #{
        context_id => CtxId,
        lib => Lib,
        map_funs => MapFuns
    },
    {ok, _, _} = ateles_client:add_map_funs(AddMapFunsReq),

    proc_lib:init_ack({ok, self()}),

    ?TRACE("ateles_worker ~p entering loop", [self()]),

    loop(CtxId).


loop(CtxId) ->
    receive
        {call, From, map_start} ->
            ?TRACE("~p map_start from ~p", [self(), From]),
            {ok, Stream} = ateles_client:map_docs(),
            gen:reply(From, ok),
            loop(CtxId, Stream);
        {call, From, stop} ->
            ?TRACE("~p stopping from ~p", [self(), From]),
            gen:reply(From, ok),
            exit(normal)
    end.


loop(CtxId, Stream) ->
    receive
        {map_doc, Id, Doc} ->
            ?TRACE("~p map_doc ~p", [self(), Id]),
            Req = #{
                context_id => CtxId,
                map_id => Id,
                doc => Doc
            },
            ok = grpcbox_client:send(Stream, Req),
            loop(CtxId, Stream);
        {call, From, {get_result, Id}} ->
            ?TRACE("~p get_result ~p : ~p", [self(), From, Id]),
            {ok, #{
                ok := Ok,
                map_id := Id,
                result := Json
            }} = grpcbox_client:recv_data(Stream),
            Status = if Ok orelse Ok == 1 -> ok; true -> error end,
            gen:reply(From, {Status, Json}),
            loop(CtxId, Stream);
        {call, From, map_end} ->
            ?TRACE("~p map_end ~p", [self(), From]),
            stream_finished = grpcbox_client:close_and_recv(Stream),
            gen:reply(From, ok),
            loop(CtxId)
    end.
