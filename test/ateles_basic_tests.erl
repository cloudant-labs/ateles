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


-include_lib("couch/include/couch_db.hrl").
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
        "Test basic ateles operations",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF(create_context),
                ?TDEF(map_docs)
            ]
        }
    }.


create_context() ->
    CtxId = couch_util:to_hex(crypto:strong_rand_bytes(16)),
    MapFuns = [
        #{
            id => <<"1">>,
            'fun' => <<"function(doc) {emit(doc.value, null);}">>
        }
    ],
    {ok, Pid} = ateles:create_map_context(CtxId, {[]}, MapFuns),
    Ref = erlang:monitor(process, Pid),
    timer:sleep(250),
    ?assert(is_process_alive(Pid)),
    ok = ateles:destroy_map_context(Pid),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            ok;
        Else ->
            erlang:error(Else)
    after 4000 ->
        erlang:error({error, process_exit_timeout})
    end.


map_docs() ->
    CtxId = couch_util:to_hex(crypto:strong_rand_bytes(16)),
    MapFuns = [
        #{
            id => <<"map_name">>,
            'fun' => <<"function(doc) {emit(doc.value, null);}">>
        }
    ],
    {ok, Ctx} = ateles:create_map_context(CtxId, {[]}, MapFuns),
    try
        Docs = lists:reverse(mkdocs(10)),
        Expect = lists:map(fun(I) ->
            [#{<<"id">> => <<"map_name">>, <<"result">> => [[I, null]]}]
        end, lists:seq(1, 10)),
        {ok, Results} = ateles:map_docs(Ctx, Docs),
        ?assertEqual(Expect, Results)
    after
        ateles:destroy_map_context(Ctx)
    end.


mkdocs(0) ->
    [];

mkdocs(N) when N > 0 ->
    Doc = #doc{
        id = integer_to_binary(N),
        body = {[
            {value, N}
        ]}
    },
    [Doc | mkdocs(N - 1)].