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
    start_link/1,
    stop/1,

    rewrite/2,
    rewrite_all/2
]).


-export([
    init/0
]).


-define(SOURCE_FILES, [
    "esprima.js",
    "escodegen.js",
    "rewrite_fun.js"
]).


start_link(_) ->
    proc_lib:start_link(?MODULE, init, []).


stop(Pid) ->
    {ok, Resp} = gen:call(Pid, stop),
    Resp.


rewrite({_CtxId, Pid}, Source) when is_binary(Source) ->
    {ok, Resp} = gen:call(Pid, call, {rewrite, Source}),
    Resp.


rewrite_all({_CtxId, Pid}, Sources) when is_list(Sources) ->
    {ok, Resp} = gen:call(Pid, call, {rewrite_all, Sources}),
    Resp.


init() ->
    proc_lib:init_ack({ok, self()}),
    {ok, Stream} = ateles_client:execute(#{channel => ateles}),
    lists:foreach(fun(FileName) ->
        {ok, _} = ateles_util:eval_file(Stream, FileName)
    end, ?SOURCE_FILES),
    loop(Stream).


loop(Stream) ->
    receive
        {call, From, {rewrite, Source}} ->
            Resp = ateles_util:call(Stream, <<"rewriteFun">>, [Source]),
            gen:reply(From, Resp),
            loop(Stream);
        {call, From, {rewrite_all, Sources}} ->
            Resp = ateles_util:call(Stream, <<"rewriteFuns">>, [Sources]),
            gen:reply(From, Resp),
            loop(Stream);
        {call, From, stop} ->
            gen:reply(From, ok),
            grpcbox_client:close_and_recv(Stream)
    end.
