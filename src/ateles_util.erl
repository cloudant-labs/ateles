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
    create_ctx/2,
    destroy_ctx/2,

    eval/4,
    eval/5,
    call/4,
    call/5,
    call_async/4,
    call_async/5,
    handle_async_resp/2,

    eval_file/3,
    load_file/1,

    fron_prompt/2,
    fron_send/2,
    fron_recv/1,

    gen_call_async/2,
    gen_recv_async/1,
    gen_recv_async/2,

    ensure_server/0
]).


create_ctx() ->
    CtxId = couch_uuids:random(),
    {ok, Conn} = ateles_conn_server:get_connection(),
    {ok, Stream} = ateles_conn_server:get_stream(Conn),
    {ok, _} = create_ctx(Stream, CtxId),
    {ok, Stream, CtxId}.


create_ctx(Stream, JSCtxId) ->
    Req = #{
        action => 'CREATE_CTX',
        context_id => JSCtxId
    },
    fron_prompt(Stream, Req).


destroy_ctx(Stream, JSCtxId) ->
    Req = #{
        action => 'DESTROY_CTX',
        context_id => JSCtxId
    },
    fron_prompt(Stream, Req).


eval(Stream, JSCtxId, FileName, Script) ->
    eval(Stream, JSCtxId, FileName, Script, 5000).


eval(Stream, JSCtxId, FileName, Script, Timeout) ->
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
    fron_prompt(Stream, Req).


call(Stream, JSCtxId, Function, Args) ->
    call(Stream, JSCtxId, Function, Args, 5000).


call(Stream, JSCtxId, Function, Args, Timeout) ->
    Req = #{
        action => 'CALL',
        context_id => JSCtxId,
        script => Function,
        args => lists:map(fun jiffy:encode/1, Args),
        timeout => Timeout
    },
    fron_prompt(Stream, Req).


call_async(Stream, JSCtxId, Function, Args) ->
    call_async(Stream, JSCtxId, Function, Args, 5000).


call_async(Stream, JSCtxId, Function, Args, Timeout) ->
    Req = #{
        action => 'CALL',
        context_id => JSCtxId,
        script => Function,
        args => lists:map(fun jiffy:encode/1, Args),
        timeout => Timeout
    },
    fron_send(Stream, Req).


handle_async_resp(Stream, Msg) ->
    {fron_stream, _Socket, _StreamId, Ref} = Stream,
    case Msg of
        {fron_msg, Ref, MsgAcc} ->
            Resp = iolist_to_binary(lists:reverse(MsgAcc)),
            case ateles_pb:decode_msg(Resp, 'JSResponse') of
                #{status := 0, result := Result} ->
                    {ok, jiffy:decode(Result)};
                #{status := Error, result := Result} ->
                    {error, {Error, Result}}
            end;
        BadMsg ->
            {exit, {invalid_msg, BadMsg}}
    end.


eval_file(Stream, JSCtxId, FileName) when is_list(FileName) ->
    Source = ateles_util:load_file(FileName),
    eval(Stream, JSCtxId, FileName, Source).


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


gen_call_async(Pid, Msg) ->
    % This is based on `gen:do_call/3`
    Ref = erlang:monitor(process, Pid),
    catch erlang:send(Pid, {call, {self(), Ref}, Msg}, [noconnect]),
    Ref.


gen_recv_async(Ref) ->
    gen_recv_async(Ref, infinity).


gen_recv_async(Ref, Timeout) ->
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            {ok, Reply};
        {'DOWN', Ref, _, _, Reason} ->
            exit(Reason)
    after Timeout ->
        erlang:demonitor(Ref, [flush]),
        exit(timeout)
    end.


fron_prompt(Stream, Req) ->
    fron_send(Stream, Req),
    fron_recv(Stream).


fron_send(Stream, Req) ->
    Msg = ateles_pb:encode_msg(Req, 'JSRequest'),
    fron:send(Stream, Msg).


fron_recv(Stream) ->
    {ok, Resp} = fron:recv(Stream, infinity),
    case ateles_pb:decode_msg(Resp, 'JSResponse') of
        #{status := 0, result := Result} ->
            {ok, jiffy:decode(Result)};
        #{status := Error, result := Result} ->
            {error, {Error, Result}}
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
    CertFile = filename:join(AppDir, "cert.pem"),
    KeyFile = filename:join(AppDir, "key.pem"),

    Args = [
        exit_status,
        {line, 1024},
        {args, [
            <<"-a">>, Host,
            <<"-p">>, Port,
            <<"--cert">>, CertFile,
            <<"--key">>, KeyFile,
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
