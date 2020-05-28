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

-module(ateles_map_doc_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(TDEF(A), {atom_to_list(A), fun A/0}).


map_doc_test_() ->
    {
        "Map doc tests",
        {
            setup,
            fun() -> test_util:start_couch([ateles]) end,
            fun test_util:stop_couch/1,
            [
                ?TDEF(map_single_doc),
                ?TDEF(map_doc_exception)
            ]
        }
    }.


map_single_doc() ->
    {ok, Ctx} = ateles:acquire_map_context(single_fun_opts()),
    {ok, Results} = ateles:map_docs(Ctx, [#doc{id = <<"foo">>}]),
    ?assertEqual([{<<"foo">>, [[{<<"foo">>, null}]]}], Results),
    ok = ateles:release_map_context(Ctx).


single_fun_opts() ->
    #{
        db_name => <<"dbname">>,
        sig => <<"sig">>,
        lib => {[]},
        map_funs => [
            <<"function(doc) {emit(doc._id, null);}">>
        ]
    }.


map_doc_exception() ->
    {ok, Ctx} = ateles:acquire_map_context(exception_opts()),
    {ok, Results} = ateles:map_docs(Ctx, [#doc{id = <<"foo">>}]),
    ?assertEqual([{<<"foo">>, [[]]}], Results),
    ok = ateles:release_map_context(Ctx).


exception_opts() ->
    #{
        db_name => <<"dbname">>,
        sig => <<"exception_sig">>,
        lib => {[]},
        map_funs => [
            <<"function(doc) {throw(new Error(\"a message\"));}">>
        ]
    }.