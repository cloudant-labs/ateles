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
-include_lib("fabric/test/fabric2_test.hrl").


timeout_test_() ->
    {
        "Timeout tests",
        {
            setup,
            fun() -> test_util:start_couch([ateles]) end,
            fun test_util:stop_couch/1,
            with([
                ?TDEF(timeout_eval_default, 10),
                ?TDEF(timeout_eval_control),
                ?TDEF(timeout_call)
            ])
        }
    }.


timeout_eval_default(_) ->
    {ok, Ctx} = ateles_util:create_ctx(),
    Script = <<"(function() {while(1) {continue;}})();">>,
    Result = ateles_util:eval({test_ctx, Ctx}, <<"foo.js">>, Script, 0),
    ?assertMatch({error, {1, <<"Time out", _/binary>>}}, Result),
    ok = ateles_util:destroy_ctx(Ctx).


timeout_eval_control(_) ->
    {ok, Ctx} = ateles_util:create_ctx(),
    Script = <<"(function() {while(1) {continue;}})();">>,
    Result = ateles_util:eval({test_ctx, Ctx}, <<"foo.js">>, Script, 250),
    ?assertMatch({error, {1, <<"Time out", _/binary>>}}, Result),
    ok = ateles_util:destroy_ctx(Ctx).


timeout_call(_) ->
    {ok, Ctx} = ateles_util:create_ctx(),
    Script = <<"function wait() {var a = 1; while(true) {a += 1;}};">>,
    {ok, _} = ateles_util:eval({test_ctx, Ctx}, <<"foo.js">>, Script, 0),
    Result = ateles_util:call({test_ctx, Ctx}, <<"wait">>, [], 250),
    ?assertMatch({error, {1, <<"Time out", _/binary>>}}, Result),
    ok = ateles_util:destroy_ctx(Ctx).
