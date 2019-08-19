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


-export([
    create_map_context/3,
    destroy_map_context/1,
    map_docs/2
]).


create_map_context(CtxId, Lib, MapFuns) ->
    LibJSON = jiffy:encode(Lib),
    ateles_worker:start_link(CtxId, LibJSON, MapFuns).


destroy_map_context(Ctx) ->
    ateles_worker:stop(Ctx).


map_docs(Ctx, Docs) ->
    ok = ateles_worker:map_start(Ctx),
    try
        Ids = lists:foldl(fun(Doc) ->
            Id = couch_util:to_hex(crypto:strong_rand_bytes(16)),
            JSON = couch_doc:to_json_obj(Doc, []),
            ok = ateles_worker:map_doc(Ctx, Id, JSON),
            Id
        end, [], Docs),
        Results = lists:foldl(fun(Id) ->
            case ateles_worker:get_result(Ctx, Id) of
                {ok, Resp} ->
                    jiffy:decode(Resp, [return_maps]);
                {error, Reason} ->
                    % log the doc id and error? blah
                    []
            end
        end, [], lists:reverse(Ids)),
        lists:reverse(Results)
    after
        ok = ateles_worker:map_stop(Ctx)
    end.