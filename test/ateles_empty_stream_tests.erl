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

-module(ateles_empty_stream_tests).


-include_lib("eunit/include/eunit.hrl").


-define(TDEF(A), {atom_to_list(A), fun A/0}).


setup() ->
    ateles_tutil:ensure_server(),
    {ok, Started} = application:ensure_all_started(ateles),
    Started.


teardown(Apps) ->
    lists:foreach(fun(App) ->
        ok = application:stop(App)
    end, lists:reverse(Apps)).


empty_streams_test_() ->
    {
        "Test basic ateles operations",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF(open_close_stream),
                ?TDEF(open_close_lots_of_streams),
                ?TDEF(multi_client_open_close_streams)
            ]
        }
    }.


open_close_stream() ->
    open_close_streams(1).


open_close_lots_of_streams() ->
    open_close_streams(1000).


multi_client_open_close_streams() ->
    Self = self(),
    Pids = lists:foldl(fun(_, Acc) ->
        Pid = spawn(fun() -> multi_client_worker(Self, 100) end),
        [Pid | Acc]
    end, [], lists:seq(1, 10)),
    lists:foreach(fun(Pid) ->
        receive
            {Pid, inited} -> ok
        end
    end, Pids),
    lists:foreach(fun(Pid) ->
        Pid ! {Self, go}
    end, Pids),
    lists:foreach(fun(Pid) ->
        receive
            {Pid, done} -> ok
        end
    end, Pids).


open_close_streams(Count) ->
    open_close_streams(Count, 10).


open_close_streams(Count, KeepOpen) ->
    OpenStreams = lists:foldl(fun(_, Streams0) ->
        {ok, Stream} = ateles_client:execute(#{channel => ateles}),
        Streams1 = [{rand:uniform(), Stream} | Streams0],

        case length(Streams1) > KeepOpen of
            true ->
                [{_, ToClose} | Streams2] = lists:sort(Streams1),
                grpcbox_client:close_and_recv(ToClose),
                Streams2;
            false ->
                Streams1
        end
    end, [], lists:seq(1, Count)),

    lists:foreach(fun({_, Stream}) ->
        grpcbox_client:close_and_recv(Stream)
    end, OpenStreams).


multi_client_worker(Parent, Count) ->
    Parent ! {self(), inited},
    receive
        {Parent, go} -> ok
    end,
    open_close_streams(Count, 1),
    Parent ! {self(), done}.
