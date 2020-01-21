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
-module(ateles_conn_server).
-behaviour(gen_server).


-export([
    start_link/0,
    get_connection/0,
    get_stream/1
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-include_lib("kernel/include/inet.hrl").


-define(CONNECT_RETRIES, 5).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_connection() ->
    gen_server:call(?MODULE, get_connection).


get_stream(Conn) ->
    fron:new_stream(Conn).


init(_) ->
    process_flag(trap_exit, true),
    {ok, fill_conns(#{conns => []})}.


terminate(_, #{conns := Conns}) ->
    lists:foreach(fun({_EP, Conn}) ->
        gen_server:cast(ateles_server, {close, Conn}),
        fron:close(Conn)
    end, Conns).


handle_call(get_connection, _From, St) ->
    {reply, {ok, random_conn(St)}, St};

handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info({'EXIT', Conn, _Reason}, St) ->
    gen_server:cast(ateles_server, {close, Conn}),
    {noreply, restart_conn(St, Conn)};

handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


fill_conns(#{conns := Conns} = St) ->
    EndPoints = get_endpoints(),
    LiveEPs = lists:foldl(fun({EP, _Pid}, EPAcc) ->
        [EPAcc | EP]
    end, [], Conns),
    NewConns = Conns ++ lists:map(fun(EP) ->
        {ok, Conn} = try_connect(EP, initial_connection, ?CONNECT_RETRIES),
        {EP, Conn}
    end, EndPoints -- LiveEPs),
    St#{conns := NewConns}.


try_connect(EP, LastError, 0) ->
    erlang:error({ateles_connection_failed, EP, LastError});

try_connect(EP, _LastError, Retries) ->
    case fron:connect(EP) of
        {ok, _} = Resp ->
            Resp;
        {error, Reason} ->
            timer:sleep(500),
            try_connect(EP, Reason, Retries - 1)
    end.


random_conn(#{conns := Conns}) ->
    {_EP, Conn} = lists:nth(rand:uniform(length(Conns)), Conns),
    Conn.


restart_conn(#{conns := Conns} = St, Conn) ->
    RestConns = [EPC || {_, C} = EPC <- Conns, C /= Conn],
    fill_conns(St#{conns := RestConns}).


get_endpoints() ->
    Host = config:get("ateles", "service_url", "localhost"),
    case Host of
        ":discover" ->
            discover_endpoints();
        _ ->
            Port = config:get_integer("ateles", "service_port", 8444),
            [to_conn_str(Host, Port)]
    end.


discover_endpoints() ->
    DNSName = config:get("ateles", "service_dns"),
    case inet_res:getbyname(DNSName, srv) of
        {ok, #hostent{h_addr_list = AddrList}} ->
            lists:map(fun({_Priority, _Weight, Port, Host}) ->
                to_conn_str(Host, Port)
            end, AddrList);
        {error, Reason} ->
            erlang:error({service_lookup_failed, Reason})
    end.


to_conn_str(Host, Port) ->
    Str = io_lib:format("ssl://~s:~b", [Host, Port]),
    lists:flatten(Str).