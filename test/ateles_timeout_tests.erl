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

-module(ateles_timeout_tests).


-include_lib("eunit/include/eunit.hrl").


-define(TDEF(A), {atom_to_list(A), fun A/0}).


timeout_test_() ->
    {
        "Timeout tests",
        {
            setup,
            fun() -> test_util:start_couch([ateles]) end,
            fun test_util:stop_couch/1,
            [
                {timeout, 10, ?TDEF(timeout_eval_default)},
                ?TDEF(timeout_eval_control),
                ?TDEF(timeout_call)
            ]
        }
    }.


timeout_eval_default() ->
    % The settings ack frame apparently isn't returned until
    % the first user generated frame is returned so we have
    % to run a first quick exchange so that chatterbox doesn't
    % blow up due to not setting the settings ack.
    {ok, Stream} = ateles_client:execute(#{channel => ateles}),
    Result1 = ateles_util:eval(Stream, <<"foo.js">>, <<"var b = 12;">>),
    ?assertMatch({ok, _}, Result1),

    Script = <<"(function() {while(1) {continue;}})();">>,
    Result2 = ateles_util:eval(Stream, <<"foo.js">>, Script, 0),
    ?assertMatch({error, {4, <<"Time out", _/binary>>}}, Result2),
    grpcbox_client:close_and_recv(Stream).


timeout_eval_control() ->
    {ok, Stream} = ateles_client:execute(#{channel => ateles}),
    Result1 = ateles_util:eval(Stream, <<"foo.js">>, <<"var b = 12;">>),
    ?assertMatch({ok, _}, Result1),

    Script = <<"(function() {while(1) {continue;}})();">>,
    Result2 = ateles_util:eval(Stream, <<"foo.js">>, Script, 250),
    ?assertMatch({error, {4, <<"Time out", _/binary>>}}, Result2),
    grpcbox_client:close_and_recv(Stream).


timeout_call() ->
    {ok, Stream} = ateles_client:execute(#{channel => ateles}),
    Script = <<"function wait() {var a = 1; while(true) {a += 1;}};">>,
    Result1 = ateles_util:eval(Stream, <<"foo.js">>, Script),
    ?assertMatch({ok, _}, Result1),

    Result2 = ateles_util:call(Stream, <<"wait">>, [], 250),
    ?assertMatch({error, {4, <<"Time out", _/binary>>}}, Result2),
    grpcbox_client:close_and_recv(Stream).
