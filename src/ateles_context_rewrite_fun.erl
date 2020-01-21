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

-module(ateles_context_rewrite_fun).
-behavior(ateles_context).


-export([
    start_link/3,
    stop/1,

    rewrite/2,
    rewrite_all/2
]).


-export([
    init/1
]).


-define(SOURCE_FILES, [
    "esprima.js",
    "escodegen.js",
    "rewrite_fun.js"
]).


-define(RETRIES, 5).


start_link(Stream, JSCtxId, _) ->
    proc_lib:start_link(?MODULE, init, [{Stream, JSCtxId}]).


stop(Pid) ->
    {ok, Resp} = gen:call(Pid, call, stop),
    Resp.


rewrite({_CtxId, Pid}, Source) when is_binary(Source) ->
    {ok, Resp} = gen:call(Pid, call, {rewrite, Source}),
    Resp.


rewrite_all({_CtxId, Pid}, Sources) when is_list(Sources) ->
    {ok, Resp} = gen:call(Pid, call, {rewrite_all, Sources}),
    Resp.


init({Conn, JSCtxId}) ->
    proc_lib:init_ack({ok, self()}),
    {ok, Stream} = ateles_conn_server:get_stream(Conn),
    {ok, _} = ateles_util:create_ctx(Stream, JSCtxId),
    lists:foreach(fun(FileName) ->
        {ok, _} = ateles_util:eval_file(Stream, JSCtxId, FileName)
    end, ?SOURCE_FILES),
    loop(Stream, JSCtxId).


loop(Stream, JSCtxId) ->
    receive
        {call, From, {rewrite, Source}} ->
            call(Stream, JSCtxId, From, <<"rewriteFun">>, [Source], ?RETRIES);
        {call, From, {rewrite_all, Sources}} ->
            call(Stream, JSCtxId, From, <<"rewriteFuns">>, [Sources], ?RETRIES);
        {call, From, stop} ->
            gen:reply(From, ok),
            ateles_util:destroy_ctx(Stream, JSCtxId)
    end.


call(_Stream, _JSCtxId, _From, _Fun, _Args, Retries) when Retries =< 0 ->
    erlang:error({failed_rewrite, retries_exhausted});

call(Stream, JSCtxId, From, Fun, Args, Retries) when Retries > 0 ->
    Resp = ateles_util:call(Stream, JSCtxId, Fun, Args),
    gen:reply(From, Resp),
    loop(Stream, JSCtxId).
