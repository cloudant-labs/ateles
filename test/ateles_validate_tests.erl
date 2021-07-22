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

-module(ateles_validate_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("fabric/test/fabric2_test.hrl").

rewrite_test_() ->
    {
        "validate tests",
        {
            setup,
            fun() -> test_util:start_couch([ateles]) end,
            fun test_util:stop_couch/1,
            with([
                ?TDEF(validate_pass),
                ?TDEF(validate_fail),
                ?TDEF(validate_bad_js_error)
            ])
        }
    }.

validate_pass(_) ->
    DDoc = create_ddoc(pass),
    Doc1 = doc(),

    Resp = ateles:validate_doc_update(DDoc, Doc1, nil, {[]}, {[]}),
    ?assertEqual(Resp, ok).

validate_fail(_) ->
    DDoc = create_ddoc(fail),
    Doc1 = doc(),

    ?assertThrow({forbidden, _}, ateles:validate_doc_update(DDoc, Doc1, nil, {[]}, {[]})).

validate_bad_js_error(_) ->
    DDoc = create_ddoc(bad_js),
    Doc1 = doc(),

    ?assertThrow({compilation_error, _}, ateles:validate_doc_update(DDoc, Doc1, nil, {[]}, {[]})).

doc() ->
    couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"doc-id">>},
            {<<"val">>, <<"value">>}
        ]}
    ).

create_ddoc(pass) ->
    JSFun = <<"function(newDoc, oldDoc, userCtx, secObj) {}">>,
    create_ddoc(JSFun);
create_ddoc(fail) ->
    JSFun = <<"function(newDoc, oldDoc, userCtx, secObj) {throw({forbidden: 'Only admins may delete other user docs.'});}">>,
    create_ddoc(JSFun);
create_ddoc(bad_js) ->
    JSFun = <<"function(newDoc, oldDoc, userCtx, secObj) { newDoc.this_does_not_exist() }">>,
    create_ddoc(JSFun);
create_ddoc(JSFun) ->
    couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"_design/doc">>},
            {<<"validate_doc_update">>, JSFun}
        ]}
    ).
