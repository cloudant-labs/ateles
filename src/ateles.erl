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


-module(ateles).
-behavior(couch_eval_impl).


-include_lib("couch/include/couch_db.hrl").


-export([
    rewrite/1,

    acquire_map_context/1,
    release_map_context/1,
    map_docs/2
]).


-define(RETRIES, 5).


rewrite(Source) when is_binary(Source) ->
    with_ctx("$rewrite$", {ateles_context_rewrite_fun, nil}, fun(Ctx) ->
        ateles_context_rewrite_fun:rewrite(Ctx, Source)
    end);

rewrite(Sources) when is_list(Sources) ->
    with_ctx("$rewrite$", {ateles_context_rewrite_fun, nil}, fun(Ctx) ->
        ateles_context_rewrite_fun:rewrite_all(Ctx, Sources)
    end).


acquire_map_context(CtxOpts) ->
    #{
        db_name := DbName,
        sig := Sig,
        lib := Lib,
        map_funs := MapFuns
    } = CtxOpts,
    CtxId = <<DbName/binary, "-mapctx-", Sig/binary>>,
    ateles_server:acquire_context(CtxId, {ateles_context_map, {Lib, MapFuns}}).


release_map_context(Ctx) ->
    ateles_server:release_context(Ctx).


map_docs(Ctx, Docs) ->
    map_docs(Ctx, Docs, ?RETRIES).


map_docs(_Ctx, _Docs, Retries) when Retries =< 0 ->
    erlang:error({map_docs_failed, retries_exhausted});

map_docs(Ctx, Docs, Retries) ->
    Refs = lists:map(fun(Doc) ->
        Json = couch_doc:to_json_obj(Doc, []),
        ateles_context_map:map_doc_async(Ctx, Json)
    end, Docs),
    try
        map_docs(Ctx, Docs, Refs, [])
    catch throw:retry ->
        map_docs(Ctx, Docs, Retries - 1)
    end.


map_docs(_Ctx, [], [], Acc) ->
    {ok, lists:reverse(Acc)};

map_docs(Ctx, [Doc | RestDocs], [Ref | RestRefs], Acc) ->
    case ateles_context_map:map_doc_recv(Ref) of
        {ok, Results} ->
            Tupled = lists:map(fun(ViewResults) ->
                lists:map(fun
                    ([K, V]) -> {K, V};
                    (Error) when is_binary(Error) -> Error
                end, ViewResults)
            end, Results),
            NewAcc = [{Doc#doc.id, Tupled} | Acc],
            map_docs(Ctx, RestDocs, RestRefs, NewAcc);
        {error, Reason} ->
            erlang:error(Reason);
        stream_finished ->
            drain_refs(RestRefs),
            throw(retry)
    end.


with_ctx(CtxId, CtxInfo, Fun) ->
    {ok, Ctx} = ateles_server:acquire_context(CtxId, CtxInfo),
    try
        Fun(Ctx)
    after
        ateles_server:release_context(Ctx)
    end.


drain_refs([]) ->
    ok;

drain_refs([Ref | Rest]) ->
    ateles_context_map:map_doc_recv(Ref),
    drain_refs(Rest).

