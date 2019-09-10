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
    eval/3,
    call/3,
    call_async/3,
    handle_async_resp/2,

    eval_file/2,
    load_file/1,

    gen_call_async/2,
    gen_recv_async/1,
    gen_recv_async/2
]).


eval(Stream, FileName, Script) ->
    Req = #{
        action => 0,
        script => Script,
        args => [
            list_to_binary("file=" ++ FileName),
            <<"line=1">>
        ]
    },
    ok = grpcbox_client:send(Stream, Req),
    {ok, Resp} = grpcbox_client:recv_data(Stream, 1000),
    case Resp of
        #{status := 0, result := Result} ->
            {ok, Result};
        #{status := Error, result := Reason} ->
            {error, {Error, Reason}}
    end.


call(Stream, Function, Args) ->
    Req = #{
        action => 1,
        script => Function,
        args => lists:map(fun jiffy:encode/1, Args)
    },
    ok = grpcbox_client:send(Stream, Req),
    {ok, Resp} = grpcbox_client:recv_data(Stream, 1000),
    case Resp of
        #{status := 0, result := Result} ->
            {ok, jiffy:decode(Result)};
        #{status := Error, result := Reason} ->
            {error, {Error, Reason}}
    end.


call_async(Stream, Function, Args) ->
    Req = #{
        action => 1,
        script => Function,
        args => lists:map(fun jiffy:encode/1, Args)
    },
    ok = grpcbox_client:send(Stream, Req).


handle_async_resp(Stream, Msg) ->
    #{
        stream_id := Id,
        stream_pid := Pid,
        monitor_ref := Ref
    } = Stream,
    case Msg of
        {headers, Id, _Resp} ->
            skip;
        {data, Id, Resp} ->
            case Resp of
                #{status := 0, result := Result} ->
                    {ok, jiffy:decode(Result)};
                #{status := Error, result := Reason} ->
                    {error, {Error, Reason}}
            end;
        {'DOWN', Ref, process, Pid, _Reason} ->
            case grpcbox_client:recv_trailers(Stream, 0) of
                {ok, {<<"0">> = _Status, _Message, _Metadata}} ->
                    {exit, stream_finished};
                {ok, {Status, Message, _Metadata}} ->
                    {exit, {Status, Message}};
                timeout ->
                    {exit, stream_finished}
            end;
        BadMsg ->
            {exit, {invalid_msg, BadMsg}}
    end.


eval_file(Stream, FileName) when is_list(FileName) ->
    Source = ateles_util:load_file(FileName),
    eval(Stream, FileName, Source).


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
