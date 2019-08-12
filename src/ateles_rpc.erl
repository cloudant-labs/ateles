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


-module(ateles_rpc).


-export([
    init/0,
    create_context/1,
    add_map_funs/1,
    map_docs/0,
    send/2,
    recv/1
]).


-type metadata() :: grpcbox:metadata().
-type empty() :: #{}.

-type error_code() :: binary().
-type error_msg() :: binary().
-type error() :: {error, {error_code(), error_msg()}}.

-type context() :: binary().

-type map_funs() :: #{context_id := context(), fun() := binary()}.
-type add_funs_opts() :: #{context_id := context(), map_funs := [map_funs()]}.

-type stream_opts() :: map().


% This isn't right, but leaving it here for now
% grpcbox needs to be configured with the correct ateles channel
% and it reads those from the environment. So at this stage I want to
% configure it here, rather than having read it from a sys.config
% This way later we can read the settings from default.ini and pass them in
% here
-spec init() -> ok.
init() ->
    Opts = {grpcbox, [
        {client, #{
            channels => [
                {ateles_js_channel, [{http, "localhost", 50051, []}], #{}}
            ]}
        }
    ]},
    application:set_env([Opts]),
    ok = application:stop(grpcbox),
    {ok, _} = application:ensure_all_started(grpcbox),
    ok.


-spec create_context(context()) -> {ok, empty(), metadata()} | error().
create_context(CtxOpts) ->
    ateles_client:create_context(CtxOpts, grpc_opts()).


-spec add_map_funs(add_funs_opts()) -> {ok, empty(), metadata()} | error().
add_map_funs(MapFunOpts) ->
    ateles_client:add_map_funs(MapFunOpts, grpc_opts()).


-spec map_docs() -> {ok, stream_opts()} | error().
map_docs() ->
    ateles_client:map_docs(grpc_opts()).


-spec send(stream_opts(), map()) -> ok | error() | {error, binary()}.
send(Stream, Data) ->
    grpcbox_client:send(Stream, Data).


-spec recv(stream_opts()) -> {ok, map()} | error() | {error, binary()}.
recv(Stream) ->
    grpcbox_client:recv_data(Stream).


grpc_opts() ->
    #{
        channel => ateles_js_channel
    }.
