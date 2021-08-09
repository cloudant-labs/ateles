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

-module(ateles_filter_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("fabric/test/fabric2_test.hrl").

rewrite_test_() ->
    {
        "filter tests",
        {
            setup,
            fun() -> test_util:start_couch([ateles]) end,
            fun test_util:stop_couch/1,
            with([
                ?TDEF(filter_view),
                ?TDEF(filter_doc),
                ?TDEF(filter_doc_error)
            ])
        }
    }.

filter_view(_) ->
    DDoc = create_ddoc(),
    Doc1 = doc(1, true),
    Doc2 = doc(2, false),
    Doc3 = doc(3, true),

    Resp = ateles:filter_view(DDoc, <<"filter">>, [Doc1, Doc2, Doc3]),
    ?assertEqual({ok, [true, false, true]}, Resp).

filter_doc(_) ->
    DDoc = create_ddoc(),
    Docs = [
        doc(1, true),
        doc(2, false),
        doc(3, true),
        doc(4, null),
        doc(5, <<"hello">>)
    ],

    JsonReq = {json_req, {[]}},
    Resp = ateles:filter_docs(JsonReq, #{}, DDoc, <<"doc-filter">>, Docs),
    ?assertEqual({ok, [true, false, true, false, true]}, Resp).

filter_doc_error(_) ->
    DDoc = create_ddoc(),
    Doc1 = doc(1, true),

    JsonReq = {json_req, {[]}},
    ?assertThrow({compilation_error, _}, ateles:filter_docs(JsonReq, #{}, DDoc, <<"error-filter">>, [Doc1])).

doc(Id, Val) ->
    couch_doc:from_json_obj(
        {[
            {<<"_id">>, list_to_binary(integer_to_list(Id))},
            {<<"val">>, Val}
        ]}
    ).

create_ddoc() ->
    couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"_design/doc">>},
            {<<"_rev">>, <<"1-12344">>},
            {<<"views">>,
                {[
                    {<<"filter">>,
                        {[
                            {<<"map">>, <<"function(doc, req) {if (doc.val) {emit(doc.val, doc.val);}}">>}
                        ]}}
                ]}},
            {<<"filters">>,
                {[
                    {<<"doc-filter">>, <<"function(doc) {return doc.val}">>},
                    {<<"error-filter">>, <<"function(doc) {boom()}">>}
                ]}}
        ]}
    ).
