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
    acquire_map_context/1,
    release_map_context/1,
    map_docs/2,
    acquire_context/0,
    release_context/1,
    try_compile/4,
    validate_doc_update/5,
    filter_view/3,
    filter_docs/5
]).


-define(REWRITE_CTX_ID, '$rewrite_context$').
-define(REWRITE_SOURCE_FILES, [
    "esprima.js",
    "escodegen.js",
    "rewrite_fun.js"
]).


acquire_map_context(CtxOpts) ->
    #{
        db_name := DbName,
        sig := Sig,
        lib := Lib,
        map_funs := RawMapFuns
    } = CtxOpts,
    CtxId = <<DbName/binary, "-mapctx-", Sig/binary>>,

    InitClosure = fun(Ctx) ->
        {ok, MapFuns} = ateles_util:rewrite(Ctx, RawMapFuns),
        case ateles_util:eval_file(Ctx, "map.js") of
            {ok, _} ->
                Args = [Lib, MapFuns],
                {ok, true} = ateles_util:call(Ctx, <<"init">>, Args),
                ok;
            {error, _} = Error ->
                Error
        end
    end,
    ateles_server:acquire(CtxId, InitClosure).


release_map_context(Ctx) ->
    ateles_server:release(Ctx).


map_docs({CtxId, _} = Ctx, Docs) ->
    {ok, pmap_docs(fun(Doc) ->
        Json = couch_doc:to_json_obj(Doc, []),
        case ateles_util:call(Ctx, <<"mapDoc">>, [Json]) of
            {ok, {ErrorProps}} when is_list(ErrorProps) ->
                Error = fabric2_util:get_value(<<"error">>, ErrorProps),
                Reason = fabric2_util:get_value(<<"reason">>, ErrorProps),
                ateles_server:destroy(Ctx),
                erlang:error({Error, Reason});
            {ok, Results} ->
                Tupled = lists:map(fun
                    (ViewResults) when is_list(ViewResults) ->
                        lists:map(fun([K, V]) -> {K, V} end, ViewResults);
                    (Error) when is_binary(Error) ->
                        Fmt = "Error mapping doc - ctx: ~s docid: ~s error: ~s",
                        couch_log:info(Fmt, [CtxId, Doc#doc.id, Error]),
                        []
                end, Results),
                {Doc#doc.id, Tupled};
            {error, Reason} ->
                erlang:error(Reason)
        end
    end, Docs)}.


acquire_context() ->
    CtxId = fabric2_util:uuid(),
    InitClosure = fun(_Ctx) -> ok end,
    ateles_server:acquire(CtxId, InitClosure).


release_context(Ctx) ->
    ateles_server:release(Ctx).

try_compile(Ctx, FunType, FunName, RawFunSrc) ->
    FunSrc = case ateles_util:rewrite(Ctx, RawFunSrc) of
        {ok, RewriteSrc} -> 
            RewriteSrc;
        {error, {_Status, RewriteResult}} ->
            compilation_error(RewriteResult, FunName, RawFunSrc);
        {error, Reason} ->
            erlang:error(Reason)
    end,
    case ateles_util:eval(Ctx, FunName, FunSrc) of
        {ok, _} ->
            ok;
        {error, {_, EvalResult}} ->
            compilation_error(EvalResult, FunType, FunName)
    end.

compilation_error(Result, FunType, FunName) ->
    Fmt = "Compilation of the ~s function in the '~s' view failed: ~p",
    Msg = io_lib:format(Fmt, [FunType, FunName, Result]),
    throw({compilation_error, Msg}).

validate_doc_update(DDoc, EditDoc, DiskDoc, UserCtx, SecObj) ->
    JsonEditDoc = couch_doc:to_json_obj(EditDoc, [revs]),
    JsonDDoc = couch_doc:to_json_obj(DDoc, []),
    JsonDiskDoc = couch_query_servers:json_doc(DiskDoc),
    ddoc_prompt(JsonDDoc, <<"validate_doc_update">>, [<<"validate_doc_update">>], [JsonEditDoc, JsonDiskDoc, UserCtx, SecObj]).

filter_view(DDoc, VName, Docs) ->
    Options = couch_query_servers:json_doc_options(),
    JsonDocs = [couch_query_servers:json_doc(Doc, Options) || Doc <- Docs],
    JsonDDoc = couch_doc:to_json_obj(DDoc, []),
    [true, Passes] = ddoc_prompt(JsonDDoc, <<"filter_view">>, [<<"views">>, VName, <<"map">>], JsonDocs),
    {ok, Passes}.

filter_docs(Req, Db, DDoc, FName, Docs) ->
    JsonReq =
        case Req of
            {json_req, JsonObj} ->
                JsonObj;
            #httpd{} = HttpReq ->
                couch_httpd_external:json_req_obj(HttpReq, Db)
        end,
    Options = couch_query_servers:json_doc_options(),
    JsonDocs = [couch_query_servers:json_doc(Doc, Options) || Doc <- Docs],
    JsonDDoc = couch_doc:to_json_obj(DDoc, []),
    [true, Passes] = ddoc_prompt(JsonDDoc, <<"filter_docs">>, [<<"filters">>, FName], [JsonReq, JsonDocs]),
    {ok, Passes}.

ddoc_prompt(DDoc, JSFun, FunName, Args) when is_list(FunName) /= true ->
    ddoc_prompt(DDoc, JSFun, [FunName], Args);
ddoc_prompt(DDoc, JSFun, FunName, Args) ->
    {ok, Ctx} = acquire_context(),
    RawFun = couch_util:get_nested_json_value(DDoc, FunName),
    {ok, Fun} = ateles_util:rewrite(Ctx, RawFun),
    try
        case ateles_util:call(Ctx, JSFun, [Fun, DDoc, Args]) of
            {ok, true} ->
                ok;
            {ok, {ErrorProps}} when is_list(ErrorProps) ->
                case ErrorProps of
                    [{<<"forbidden">>, Message}] ->
                        throw({forbidden, Message});
                    [{<<"unauthorized">>, Message}] ->
                        throw({unauthorized, Message});
                    [{<<"compilation_error">>, Message}] ->
                        throw({compilation_error, Message});
                    [{_, Message}] ->
                        throw({unknown_error, Message});
                    Message when is_binary(Message) ->
                        throw({unknown_error, Message})
                end;
            {ok, [true, Passes]} ->
                [true, Passes];
            {ok, Unknown} ->
                Fmt = "prompt doc unknown response- ~p",
                couch_log:info(Fmt, [Unknown]),
                throw({unknown_error, <<"unknown error">>});
            {error, Reason} ->
                erlang:error(Reason)
        end
    after
        release_context(Ctx)
    end.

pmap_docs(Fun, Items) ->
    Parent = self(),
    Entries = lists:map(fun(Item) ->
        spawn_monitor(fun() ->
            Parent ! {self(), Fun(Item)}
        end)
    end, Items),
    collect(Entries, 5000).


collect([], _Timeout) ->
    [];

collect([{Pid, Ref} | Rest], Timeout) ->
    receive
        {Pid, Result} ->
            erlang:demonitor(Ref, [flush]),
            [Result | collect(Rest, Timeout)];
        {'DOWN', Ref, process, Pid, Error} ->
            erlang:error({map_docs, Error})
    after Timeout ->
        erlang:error({pmap_docs_error, timeout})
    end.
