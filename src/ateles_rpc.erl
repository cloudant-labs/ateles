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
    map_docs/0
]).


-type id() :: binary().
-type context() :: #{id() := binary()}.


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


-spec create_context(context) -> {ok, map(), grpcbox:metadata()} |
    {error, binary()}.
create_context(CtxOpts) ->
    ateles_client:create_context(CtxOpts, grpc_opts()).


add_map_funs(MapFunOpts) ->
    ateles_client:add_map_funs(MapFunOpts, grpc_opts()).


map_docs() ->
    ateles_client:map_docs(grpc_opts()).


grpc_opts() ->
    #{
        channel => ateles_js_channel
    }.
