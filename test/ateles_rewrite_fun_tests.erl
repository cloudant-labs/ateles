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

-module(ateles_rewrite_fun_tests).


-include_lib("eunit/include/eunit.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


rewrite_test_() ->
    {
        "Rewrite tests",
        {
            setup,
            fun() -> test_util:start_couch([ateles]) end,
            fun test_util:stop_couch/1,
            with([
                ?TDEF(rewrite_simple),
                ?TDEF(rewrite_all),
                ?TDEF(rewrite_valid)
            ])
        }
    }.


rewrite_simple(_) ->
    Function = <<"function(doc) {emit(null, null);}">>,
    Expect = <<"(function (doc) {\n    emit(null, null);\n});">>,
    {ok, Result} = do_rewrite(Function),
    ?assertEqual(Expect, Result).


rewrite_all(_) ->
    Functions = [
        <<"function(doc){ emit(null, null);}">>,
        <<"function(bar){return;}">>
    ],
    Expect = [
        <<"(function (doc) {\n    emit(null, null);\n});">>,
        <<"(function (bar) {\n    return;\n});">>
    ],
    {ok, Result} = do_rewrite(Functions),
    ?assertEqual(Expect, Result).


rewrite_valid(_) ->
    Function = <<"(function (doc) {\n    emit(null, null);\n});">>,
    {ok, Result} = do_rewrite(Function),
    ?assertEqual(Function, Result).


do_rewrite(Function) ->
    {ok, JSCtx} = ateles_util:create_ctx(),
    try
        ateles_util:rewrite({test_ctx, JSCtx}, Function)
    after
        ateles_util:destroy_ctx(JSCtx)
    end.

