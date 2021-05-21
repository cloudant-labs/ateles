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

-module(ateles_server).


-behaviour(gen_server).


-export([
    start_link/0,

    acquire/2,
    release/1,
    destroy/1
]).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-include_lib("couch/include/couch_db.hrl").


-define(CONTEXTS, ateles_server_contexts).
-define(CLIENTS, ateles_server_clients).
-define(LRU, ateles_server_lru).
-define(MAX_CONTEXTS, 50).

-ifdef(TEST).
-define(VALIDATE(Step), validate_tables(Step)).
-else.
-define(VALIDATE(Step), ok).
-endif.


-record(ctx, {
    ctx_id,
    js_ctx,
    http_pid,
    ref_count,
    last_use
}).

-record(client, {
    pid,
    ref,
    ctx_id,
    ref_count
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


acquire(CtxId, InitClosure) ->
    gen_server:call(?MODULE, {acquire, CtxId, InitClosure}, infinity).


release({CtxId, _JSCtx}) ->
    gen_server:call(?MODULE, {release, CtxId}, infinity).


destroy({_CtxId, JSCtx}) ->
    ateles_util:destroy_ctx(JSCtx).


init(_) ->
    process_flag(trap_exit, true),
    ets:new(?CONTEXTS, [set, protected, named_table, {keypos, #ctx.ctx_id}]),
    ets:new(?CLIENTS, [bag, protected, named_table, {keypos, #client.pid}]),
    ets:new(?LRU, [ordered_set, protected, named_table]),
    {ok, #{
        max_contexts => max_contexts()
    }}.


terminate(_, _St) ->
    ets:foldl(fun(Ctx, _) ->
        #ctx{
            ctx_id = CtxId,
            js_ctx = JSCtx
        } = Ctx,
        ateles_util:destroy_ctx({CtxId, JSCtx})
    end, nil, ?CONTEXTS),
    ok.


handle_call({acquire, CtxId, InitClosure}, {ClientPid, _Tag}, St) ->
    ?VALIDATE(acquire_start),
    case acquire_int(CtxId, InitClosure, St) of
        {ok, JSCtx} ->
            ets:update_counter(?CONTEXTS, CtxId, {#ctx.ref_count, 1}),
            Pattern = #client{pid = ClientPid, ctx_id = CtxId, _ = '_'},
            Client0 = case ets:match_object(?CLIENTS, Pattern) of
                [] ->
                    #client{
                        pid = ClientPid,
                        ref = erlang:monitor(process, ClientPid),
                        ctx_id = CtxId,
                        ref_count = 0
                    };
                [C] ->
                    ets:delete_object(?CLIENTS, C),
                    C
            end,
            Client1 = Client0#client{
                ref_count = 1 + Client0#client.ref_count
            },
            ets:insert(?CLIENTS, Client1),
            ?VALIDATE(acquire_success),
            {reply, {ok, {CtxId, JSCtx}}, St};
        Error ->
            ?VALIDATE(acquire_error),
            {reply, Error, St}
    end;

handle_call({release, CtxId}, {ClientPid, _Tag}, St) ->
    ?VALIDATE(release_start),

    CtxPattern = #ctx{ctx_id = CtxId, _ = '_'},
    case ets:match_object(?CONTEXTS, CtxPattern) of
        [] ->
            {reply, ok, St};
        [_Ctx] ->
            ClientPattern = #client{pid = ClientPid, ctx_id = CtxId, _ = '_'},
            [Client] = ets:match_object(?CLIENTS, ClientPattern),

            release_int(Client, 1),
            erlang:demonitor(Client#client.ref, [flush]),

            ?VALIDATE(release_end),
            {reply, ok, St}
    end;

handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info({'DOWN', _Ref, process, ClientPid, _Reason}, St) ->
    ?VALIDATE(client_down_start),

    Pattern = #client{pid = ClientPid, _ = '_'},
    lists:foreach(fun(Client) ->
        release_int(Client, Client#client.ref_count),
        erlang:demonitor(Client#client.ref, [flush])
    end, ets:match_object(?CLIENTS, Pattern)),

    ?VALIDATE(client_down_end),
    {noreply, St};

handle_info({'EXIT', Pid, _Reason}, St) ->
    ?VALIDATE(http_pid_exit_start),

    CtxPattern = #ctx{http_pid = Pid, _ = '_'},
    case ets:match_object(?CONTEXTS, CtxPattern) of
        [#ctx{ctx_id = CtxId, last_use = undefined}] ->
            ClientPattern = #client{ctx_id = CtxId, _ = '_'},
            lists:foreach(fun(Client) ->
                #client{
                    ref = Ref
                } = Client,
                ets:delete_object(?CLIENTS, Client),
                erlang:demonitor(Ref, [flush])
            end, ets:match_object(?CLIENTS, ClientPattern)),
            ets:delete(?CONTEXTS, CtxId);
        [#ctx{ctx_id = CtxId, last_use = LU}] ->
            ets:delete(?CONTEXTS, CtxId),
            ets:delete(?LRU, LU);
        [] ->
            ok
    end,

    ?VALIDATE(http_pid_exit_end),
    {noreply, St};

handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


acquire_int(CtxId, InitClosure, #{max_contexts := MaxContexts} = St) ->
    NumContexts = ets:info(?CONTEXTS, size),
    case ets:lookup(?CONTEXTS, CtxId) of
        [#ctx{js_ctx=JSCtx, ref_count=N, last_use=undefined}] when N > 0 ->
            {ok, JSCtx};
        [#ctx{js_ctx=JSCtx, ref_count=0, last_use=LU}] ->
            ets:update_element(?CONTEXTS, CtxId, {#ctx.last_use, undefined}),
            ets:delete(?LRU, LU),
            {ok, JSCtx};
        [] when NumContexts < MaxContexts ->
            {ok, {_EndPoint, HttpPid} = JSCtx} = ateles_util:create_ctx(),
            Ctx = #ctx{
                ctx_id = CtxId,
                js_ctx = JSCtx,
                http_pid = HttpPid,
                ref_count = 0,
                last_use = undefined
            },
            try InitClosure({CtxId, JSCtx}) of
                ok ->
                    ets:insert(?CONTEXTS, Ctx),
                    {ok, JSCtx};
                {error, Reason} = Error ->
                    Fmt = "Failed to initialize ateles context: ~p",
                    couch_log:error(Fmt, [Reason]),
                    Error
            catch ?STACKTRACE(T, R, S) ->,
                Fmt = "Failed to initialize ateles context: ~p",
                couch_log:error(Fmt, [{T, R, S}]),
                {error, {T, R, S}}
            end;
        [] ->
            case remove_context() of
                true ->
                    acquire_int(CtxId, InitClosure, St);
                false ->
                    couch_stats:increment_counter([ateles, all_contexts_active_acquire_failure]),
                    {error, all_contexts_active}
            end
    end.


release_int(#client{} = Client, Decrement) ->
    #client{
        ctx_id = CtxId,
        ref_count = RefCount
    } = Client,
    DecOp = {#ctx.ref_count, -1 * Decrement},
    CtxRC = ets:update_counter(?CONTEXTS, CtxId, DecOp),
    true = ets:delete_object(?CLIENTS, Client),

    if RefCount == Decrement -> ok; true ->
        ets:insert(?CLIENTS, Client#client{ref_count = RefCount - Decrement})
    end,

    % Blow up if we find an invalid ref count
    if CtxRC >= 0 -> ok; true ->
        erlang:error({invalid_context_ref_count, CtxId})
    end,

    if CtxRC /= 0 -> ok; true ->
        % RefCount dropped below zero so place this context
        % into the removable pool
        LastUse = erlang:monotonic_time(),
        ets:update_element(?CONTEXTS, CtxId, {#ctx.last_use, LastUse}),
        ets:insert(?LRU, {LastUse, CtxId})
    end.


remove_context() ->
    case ets:first(?LRU) of
        '$end_of_table' ->
            false;
        Timestamp ->
            [{_, CtxId}] = ets:lookup(?LRU, Timestamp),
            [Ctx] = ets:lookup(?CONTEXTS, CtxId),
            #ctx{
                js_ctx = JSCtx,
                ref_count = 0
            } = Ctx,
            ok = ateles_util:destroy_ctx(JSCtx),
            ets:delete(?CONTEXTS, CtxId),
            ets:delete(?LRU, Timestamp),
            true
    end.


max_contexts() ->
    config:get_integer("ateles", "max_contexts", ?MAX_CONTEXTS).


-ifdef(TEST).
validate_tables(Step) ->
    try
        validate_tables()
    catch T:R ->
        erlang:error({Step, T, R})
    end.


validate_tables() ->
    % Count all of our expected ref counts
    RefCounts = ets:foldl(fun(Client, Acc) ->
        #client{
            ctx_id = CtxId,
            ref_count = RefCount
        } = Client,
        true = RefCount > 0,
        maps:update_with(CtxId, fun(RC) -> RC + RefCount end, RefCount, Acc)
    end, #{}, ?CLIENTS),

    % Assert any ref count in ?CONTEXTS matches
    % the value found in ?CLIENTS
    ets:foldl(fun(Ctx, _) ->
        #ctx{
            ctx_id = CtxId,
            ref_count = RefCount
        } = Ctx,
        true = RefCount >= 0,
        RefCount = maps:get(CtxId, RefCounts, 0)
    end, nil, ?CONTEXTS),

    % Assert any ref count discovered in ?CLIENTS
    % is accurate in ?CONTEXTS
    maps:fold(fun(CtxId, RefCount, _) ->
        [Ctx] = ets:lookup(?CONTEXTS, CtxId),
        #ctx{
            ref_count = RC
        } = Ctx,
        RC = RefCount
    end, nil, RefCounts),

    % Assert that the `{ClientPid, CtxId}` pairs in
    % ?CLIENTS are unique. We could have duplicates if
    % they only differed by a ref_count
    ets:foldl(fun(Client, Acc) ->
        Pair = {Client#client.pid, Client#client.ctx_id},
        false = lists:member(Pair, Acc),
        [Pair | Acc]
    end, [], ?CLIENTS),

    % If a context has a ref count of 0, it must have
    % a timestamp set in the LRU
    ets:foldl(fun(Ctx, _) ->
        case Ctx of
            #ctx{ctx_id = CtxId, ref_count = 0, last_use = LU} ->
                [{LU, CtxId}] = ets:lookup(?LRU, LU);
            #ctx{ref_count = N, last_use = undefined} when N > 0 ->
                ok
        end
    end, nil, ?CONTEXTS),

    % Assert every entry in the LRU has a ref count
    % of zero in ?CONTEXTS and ?CLIENTS. The check for
    % ?CLIENTS relies on our previously computed
    % RefCounts map.
    ets:foldl(fun({LU, CtxId}, _) ->
        [Ctx] = ets:lookup(?CONTEXTS, CtxId),
        #ctx{
            ref_count = 0,
            last_use = LU
        } = Ctx,
        false = maps:is_key(CtxId, RefCounts)
    end, nil, ?LRU),

    ok.
-endif.
