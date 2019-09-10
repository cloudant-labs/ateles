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


-module(ateles_tutil).


-export([
    ensure_server/0
]).



ensure_server() ->
    case application:get_env(ateles, server) of
        undefined ->
            Pid = spawn(fun() -> run_server() end),
            application:set_env(ateles, server, Pid);
        _ ->
            ok
    end.


run_server() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    Command = filename:join(PrivDir, "ateles"),
    Args = [
        exit_status,
        {line, 1024},
        {args, [
            <<"-n">>, <<"2">>,
            <<"-p">>, list_to_binary(os:getpid())
        ]
    }],
    Port = erlang:open_port({spawn_executable, Command}, Args),
    server_loop(Port).


server_loop(Port) ->
    receive
        {Port, {data, _}} ->
            server_loop(Port);
        Error ->
            io:format(standard_error, "SERVER ERROR: ~p~n", [Error])
    end.
