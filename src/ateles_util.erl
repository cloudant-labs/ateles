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
    new_js_ctx/0,

    create_ctx/1,
    create_test_ctx/0,
    destroy_ctx/1,

    eval/3,
    eval/4,
    call/3,
    call/4,

    eval_file/2,
    load_file/1,

    ensure_server/0
]).



new_js_ctx() ->
    EndPoint = get_endpoint(),
    {ok, Pid} = ibrowse:spawn_link_worker_process(EndPoint),
    JSCtxId = couch_uuids:random(),
    {ok, {Pid, EndPoint, JSCtxId}}.


create_ctx({_CtxId, {Pid, EndPoint, JSCtxId}}) ->
    Req = #{
        action => 'CREATE_CTX',
        context_id => JSCtxId
    },
    prompt(Pid, EndPoint, Req).


create_test_ctx() ->
    {ok, JSCtx} = new_js_ctx(),
    {ok, true} = create_ctx({ignored, JSCtx}),
    {ok, {ignored, JSCtx}}.


destroy_ctx({_CtxId, {Pid, EndPoint, JSCtxId}}) ->
    Req = #{
        action => 'DESTROY_CTX',
        context_id => JSCtxId
    },
    Resp = prompt(Pid, EndPoint, Req),
    ok = ibrowse:stop_worker_process(Pid),
    Resp.


eval(Ctx, FileName, Script) ->
    eval(Ctx, FileName, Script, 5000).


eval({_CtxId, {Pid, EndPoint, JSCtxId}}, FileName, Script, Timeout) ->
    Req = #{
        action => 'EVAL',
        context_id => JSCtxId,
        script => Script,
        args => [
            list_to_binary("file=" ++ FileName),
            <<"line=1">>
        ],
        timeout => Timeout
    },
    prompt(Pid, EndPoint, Req).


call(Ctx, Function, Args) ->
    call(Ctx, Function, Args, 5000).


call({_CtxId, {Pid, EndPoint, JSCtxId}}, Function, Args, Timeout) ->
    Req = #{
        action => 'CALL',
        context_id => JSCtxId,
        script => Function,
        args => lists:map(fun jiffy:encode/1, Args),
        timeout => Timeout
    },
    prompt(Pid, EndPoint, Req).


eval_file(Ctx, FileName) when is_list(FileName) ->
    Source = ateles_util:load_file(FileName),
    eval(Ctx, FileName, Source).


load_file(FileName) when is_list(FileName) ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        PDir ->
            PDir
    end,
    Path = filename:join(PrivDir, FileName),
    {ok, Data} = file:read_file(Path),
    Data.


get_endpoint() ->
    case application:get_env(ateles, endpoints) of
        {ok, EndPoints} ->
            lists:nth(rand:uniform(length(EndPoints)), EndPoints);
        _ ->
            erlang:error({ateles, no_endpoints_configured})
    end.


prompt(Pid, EndPoint, Req) ->
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


run_server() ->
    AppDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            filename:dirname(EbinDir);
        Path ->
            Path
    end,
    PrivDir = filename:join(AppDir, "priv"),

    Command = filename:join(PrivDir, "ateles"),
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
