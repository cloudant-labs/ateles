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

-module(ateles_acquire_release_tests).


-include_lib("eunit/include/eunit.hrl").


-define(TDEF(A), {atom_to_list(A), fun A/0}).


-define(CONTEXTS, ateles_server_contextss).
-define(CLIENTS, ateles_server_clients).
-define(LRU, ateles_server_lru).

-define(CTX_ID, '$test_context$').
-define(INIT_CLOSURE, fun(_) -> ok end).


acquire_release_test_() ->
    {
        "Acquire/Release tests",
        {
            setup,
            fun() -> test_util:start_couch([ateles]) end,
            fun test_util:stop_couch/1,
            [
                ?TDEF(acquire_release),
                ?TDEF(acquire_multiple)
            ]
        }
    }.


acquire_release() ->
    {ok, Ctx} = ateles_server:acquire(?CTX_ID, ?INIT_CLOSURE),
    ?assertEqual(1, length(ets:lookup(?CLIENTS, self()))),
    ok = ateles_server:release(Ctx),
    ?assertEqual(0, length(ets:lookup(?CLIENTS, self()))).


acquire_multiple() ->
    {ok, Ctx1} = ateles_server:acquire(?CTX_ID, ?INIT_CLOSURE),
    {ok, Ctx2} = ateles_server:acquire(?CTX_ID, ?INIT_CLOSURE),

    ?assertEqual(Ctx1, Ctx2),
    ?assertEqual(1, length(ets:lookup(?CLIENTS, self()))),

    ok = ateles_server:release(Ctx1),
    ?assertEqual(1, length(ets:lookup(?CLIENTS, self()))),

    ok = ateles_server:release(Ctx2),
    ?assertEqual(0, length(ets:lookup(?CLIENTS, self()))).
