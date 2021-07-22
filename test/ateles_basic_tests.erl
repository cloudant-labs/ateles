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
-include_lib("fabric/test/fabric2_test.hrl").


basic_test_() ->
    {
        "Basic tests",
        {
            setup,
            fun() -> test_util:start_couch([ateles]) end,
            fun test_util:stop_couch/1,
            with([
                ?TDEF(eval_code),
                ?TDEF(call_function)
            ])
        }
    }.


eval_code(_) ->
    {ok, Ctx} = ateles_util:create_ctx(),
    Script = <<"var x = 2; x;">>,
    {ok, 2} = ateles_util:eval({test_ctx, Ctx}, <<"foo.js">>, Script),
    ok = ateles_util:destroy_ctx(Ctx).


call_function(_) ->
    {ok, Ctx} = ateles_util:create_ctx(),
    Script = <<"function double(x) {return x * 2;};">>,
    {ok, _} = ateles_util:eval({test_ctx, Ctx}, <<"foo.js">>, Script),
    {ok, 4} = ateles_util:call({test_ctx, Ctx}, <<"double">>, [2]),
    ok = ateles_util:destroy_ctx(Ctx).
