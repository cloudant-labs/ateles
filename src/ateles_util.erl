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


-module(ateles_util).


-export([
    create_ctx/0,
    destroy_ctx/1,

    rewrite/2,
    rewrite/3,
    eval/3,
    eval/4,
    call/3,
    call/4,

    eval_file/2,
    load_file/1,

    ensure_server/0
]).



create_ctx() ->
    EndPoint = get_endpoint(),
    {ok, Pid} = ibrowse:spawn_link_worker_process(EndPoint),
    {ok, {EndPoint, Pid}}.


destroy_ctx({_EndPoint, Pid}) ->
    ok = ibrowse:stop_worker_process(Pid).


rewrite(Ctx, Sources) ->
    rewrite(Ctx, Sources, 5000).


rewrite({_CtxId, JSCtx}, Source, Timeout) when is_binary(Source) ->
    Req = #{
        action => 'REWRITE',
        script => <<"rewriteFun">>,
        args => [jiffy:encode(Source)],
        timeout => Timeout
    },
    prompt(JSCtx, Req);

rewrite({_CtxId, JSCtx}, Sources, Timeout) when is_list(Sources) ->
    Req = #{
        action => 'REWRITE',
        script => <<"rewriteFuns">>,
        args => [jiffy:encode(Sources)],
        timeout => Timeout
    },
    prompt(JSCtx, Req).


eval(Ctx, FileName, Script) ->
    eval(Ctx, FileName, Script, 5000).


eval({_CtxId, JSCtx}, FileName, Script, Timeout) ->
    Req = #{
        action => 'EVAL',
        script => Script,
        args => [
            list_to_binary("file=" ++ FileName),
            <<"line=1">>
        ],
        timeout => Timeout
    },
    prompt(JSCtx, Req).


call(Ctx, Function, Args) ->
    call(Ctx, Function, Args, 5000).


call({_CtxId, JSCtx}, Function, Args, Timeout) ->
    Req = #{
        action => 'CALL',
        script => Function,
        args => lists:map(fun jiffy:encode/1, Args),
        timeout => Timeout
    },
    prompt(JSCtx, Req).


eval_file(Ctx, FileName) when is_list(FileName) ->
    Source = ateles_util:load_file(FileName),
    eval(Ctx, FileName, Source).


load_file(FileName) when is_list(FileName) ->
    Path = filename:join(priv_dir(), FileName),
    {ok, Data} = file:read_file(Path),
    Data.


get_endpoint() ->
    case application:get_env(ateles, endpoints) of
        {ok, EndPoints} ->
            lists:nth(rand:uniform(length(EndPoints)), EndPoints);
        _ ->
            erlang:error({ateles, no_endpoints_configured})
    end.


prompt({EndPoint, Pid}, Req) ->
    URL = EndPoint ++ "/Ateles/Execute",
    ReqBin = ateles_pb:encode_msg(Req, 'JSRequest'),
    Opts = [{response_format, binary}],
    case ibrowse:send_req_direct(Pid, URL, [], post, ReqBin, Opts) of
        {ok, "200", _Headers, RespBin} ->
            case ateles_pb:decode_msg(RespBin, 'JSResponse') of
                #{status := 0, result := Result} ->
                    {ok, jiffy:decode(Result)};
                #{status := Status, result := Result} ->
                    {error, {Status, Result}}
            end;
        {ok, Code, _, _} ->
            {error, {ateles_protocol_error, Code}};
        {error, Reason} ->
            {error, Reason}
    end.


ensure_server() ->
    case application:get_env(ateles, server) of
        undefined ->
            Pid = spawn(fun() -> run_server() end),
            application:set_env(ateles, server, Pid);
        _ ->
            ok
    end.


run_server(Parent) ->
    Command = filename:join(priv_dir(), "ateles"),
    Host = config:get("ateles", "service_url", "127.0.0.1"),
    Port = config:get("atelese", "service_port", "8444"),

    Args = [
        exit_status,
        {line, 1024},
        {args, [
            <<"-a">>, Host,
            <<"-p">>, Port,
            <<"--parent_pid">>, list_to_binary(os:getpid())
        ]
    }],
    ServerPort = erlang:open_port({spawn_executable, Command}, Args),
    server_loop(ServerPort).


server_loop(Port) ->
    receive
        {Port, {data, _}} ->
            server_loop(Port);
        Error ->
            io:format(standard_error, "SERVER ERROR: ~p~n", [Error])
    end.


priv_dir() ->
    case code:priv_dir(ateles) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        PrivDir ->
            PrivDir
    end.
