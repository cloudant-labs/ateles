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


-include_lib("kernel/include/inet.hrl").


start(_, _) ->
    {ok, _} = application:ensure_all_started(grpcbox),
    case application:get_env(fabric, eunit_run) of
        {ok, true} ->
            ateles_util:ensure_server();
        _ ->
            ok
    end,
    Endpoints = get_endpoints(),
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


get_endpoints() ->
    Host = config:get("ateles", "service_url", "localhost"),
    case Host of
        ":discover" ->
            discover_endpoints();
        _ ->
            Port = config:get_integer("ateles", "service_port", 8444),
            [{http, Host, Port, []}]
    end.


discover_endpoints() ->
    DNSName = config:get("ateles", "service_dns"),
    case inet_res:getbyname(DNSName, srv) of
        {ok, #hostent{h_addr_list = AddrList}} ->
            lists:map(fun({_Priority, _Weight, Port, Host}) ->
                [{http, Host, Port, []}]
            end, AddrList);
        {error, Reason} ->
            erlang:error({service_lookup_failed, Reason})
    end.
