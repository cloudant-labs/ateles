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

-module(ateles_basic_tests).


-include_lib("eunit/include/eunit.hrl").


-define(TDEF(A), {atom_to_list(A), fun A/0}).


setup() ->
    {ok, Started} = application:ensure_all_started(ateles),
    Started.


teardown(Apps) ->
    lists:foreach(fun(App) ->
        ok = application:stop(App)
    end, lists:reverse(Apps)).


basic_test_() ->
    {
        "Basic tests",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF(eval_code),
                ?TDEF(call_function)
            ]
        }
    }.


eval_code() ->
    {ok, Stream} = ateles_client:execute(#{channel => ateles}),
    Req = #{
        action => 0,
        script => <<"var x = 2; x;">>,
        args => [<<"file=foo">>, <<"line=1">>]
    },
    ok = grpcbox_client:send(Stream, Req),
    {ok, Result} = grpcbox_client:recv_data(Stream),
    ?assertEqual(#{status => 0, result => <<"2">>}, Result),
    grpcbox_client:close_and_recv(Stream).


call_function() ->
    {ok, Stream} = ateles_client:execute(#{channel => ateles}),
    Req1 = #{
        action => 0,
        script => <<"function double(x) {return x * 2;};">>,
        args => []
    },
    ok = grpcbox_client:send(Stream, Req1),
    {ok, Result1} = grpcbox_client:recv_data(Stream),
    ?assertMatch(#{status := 0, result := <<_/binary>>}, Result1),

    Req2 = #{
        action => 1,
        script => <<"double">>,
        args => [<<"2">>]
    },

    ok = grpcbox_client:send(Stream, Req2),
    {ok, Result2} = grpcbox_client:recv_data(Stream),
    ?assertEqual(#{status => 0, result => <<"4">>}, Result2),
    grpcbox_client:close_and_recv(Stream).


