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

-module(ateles_app).
-behaviour(application).


-export([
    start/2,
    stop/1
]).


start(_, _) ->
    {ok, _} = application:ensure_all_started(grpcbox),
    case application:get_env(fabric, eunit_run) of
        {ok, true} ->
            ateles_util:ensure_server();
        _ ->
            ok
    end,
    Host = config:get("ateles", "service_url", "localhost"),
    Port = config:get_integer("ateles", "service_port", 8444),
    Endpoints = [{http, Host, Port, []}],
    {ok, Pid} = grpcbox_channel_sup:start_child(ateles, Endpoints, #{}),
    ok = application:set_env(ateles, channel_pid, Pid),
    ateles_sup:start_link().


stop(_) ->
    {ok, Pid} = application:get_env(ateles, channel_pid),
    case is_pid(Pid) andalso is_process_alive(Pid) of
        true -> grpcbox_channel:stop(Pid);
        false -> ok
    end,
    ok.
