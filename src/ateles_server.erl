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

    acquire_context/2,
    release_context/1
]).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-define(WORKERS, ateles_server_workers).
-define(CLIENTS, ateles_server_clients).
-define(LRU, ateles_server_lru).
-define(MAX_WORKERS, 50).

-ifdef(TEST).
-define(VALIDATE(Step), validate_tables(Step)).
-else.
-define(VALIDATE(Step), ok).
-endif.


-record(worker, {
    ctx_id,
    ctx_mod,
    pid,
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


acquire_context(CtxId, CtxInfo) ->
    gen_server:call(?MODULE, {acquire, CtxId, CtxInfo}).


release_context({CtxId, Pid}) ->
    gen_server:call(?MODULE, {release, CtxId, Pid}).


init(_) ->
    process_flag(trap_exit, true),
    ets:new(?WORKERS, [set, protected, named_table, {keypos, #worker.ctx_id}]),
    ets:new(?CLIENTS, [bag, protected, named_table, {keypos, #client.pid}]),
    ets:new(?LRU, [ordered_set, protected, named_table]),
    {ok, #{
        max_workers => max_workers()
    }}.


terminate(_, _St) ->
    ets:foldl(fun(Worker, _) ->
        #worker{
            ctx_mod = CtxMod,
            pid = Pid
        } = Worker,
        CtxMod:stop(Pid)
    end, nil, ?WORKERS),
    ok.


handle_call({acquire, CtxId, CtxInfo}, {ClientPid, _Tag}, St) ->
    ?VALIDATE(acquire_start),
    case acquire(CtxId, CtxInfo, St) of
        {ok, Pid} ->
            ets:update_counter(?WORKERS, CtxId, {#worker.ref_count, 1}),
            Pattern = #client{pid = ClientPid, ctx_id = CtxId, _ = '_'},
            Client0 = case ets:lookup(?CLIENTS, Pattern) of
                [] ->
                    #client{
                        pid = ClientPid,
                        ref = erlang:monitor(process, ClientPid),
                        ctx_id = CtxId,
                        ref_count = 0
                    };
                [C] ->
                    C
            end,
            Client1 = Client0#client{
                ref_count = 1 + Client0#client.ref_count
            },
            ets:insert(?CLIENTS, Client1),
            ?VALIDATE(acquire_success),
            {reply, {ok, {CtxId, Pid}}, St};
        Error ->
            ?VALIDATE(acquire_error),
            {reply, Error, St}
    end;

handle_call({release, CtxId, CtxPid}, {ClientPid, _Tag}, St) ->
    ?VALIDATE(release_start),

    WorkerPattern = #worker{ctx_id = CtxId, pid = CtxPid, _ = '_'},
    case ets:match_object(?WORKERS, WorkerPattern) of
        [] ->
            {reply, ok, St};
        [_Worker] ->
            ClientPattern = #client{pid = ClientPid, ctx_id = CtxId, _ = '_'},
            [Client] = ets:match_object(?CLIENTS, ClientPattern),

            release(Client),
            erlang:demonitor(Client#client.ref, [flush]),

            ?VALIDATE(release_end),
            {reply, ok, St}
    end;

handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info({'EXIT', _CtxPid, normal}, St) ->
    % This was a process that we asked to shutdown
    % so it can be ignored.
    ?VALIDATE(worker_exit_normal),
    {noreply, St};

handle_info({'EXIT', CtxPid, _Reason}, St) ->
    ?VALIDATE(worker_exit_start),
    [Worker] = ets:match_object(?WORKERS, #worker{pid = CtxPid, _ = '_'}),
    #worker{
        ctx_id = CtxId,
        ref_count = RefCount,
        last_use = LU
    } = Worker,

    ets:delete(?WORKERS, CtxId),

    Clients = ets:match_object(?CLIENTS, #client{ctx_id = CtxId, _ = '_'}),
    lists:foreach(fun(Client) ->
        ets:delete_object(?CLIENTS, Client),
        erlang:demonitor(Client#client.ref, [flush])
    end, Clients),

    if RefCount > 0 -> ok; true ->
        ets:delete(?LRU, LU)
    end,

    ?VALIDATE(worker_exit_end),
    {noreply, St};

handle_info({'DOWN', _Ref, process, ClientPid, _Reason}, St) ->
    ?VALIDATE(client_down_start),

    Pattern = #client{pid = ClientPid, _ = '_'},
    lists:foreach(fun(Client) ->
        release(Client),
        erlang:demonitor(Client#client.ref, [flush])
    end, ets:match_object(?CLIENTS, Pattern)),

    ?VALIDATE(client_down_end),
    {reply, ok, St};

handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


acquire(CtxId, {CtxMod, CtxArg}, #{max_workers := MaxWorkers} = St) ->
    NumWorkers = ets:info(?WORKERS, size),
    case ets:lookup(?WORKERS, CtxId) of
        [#worker{pid = Pid, ref_count = N, last_use = undefined}] when N > 0 ->
            {ok, Pid};
        [#worker{pid = Pid, ref_count = 0, last_use = LU}] ->
            ets:update_element(?WORKERS, CtxId, {#worker.last_use, undefined}),
            ets:delete(?LRU, LU),
            {ok, Pid};
        [] when NumWorkers < MaxWorkers ->
            {ok, CtxPid} = CtxMod:start_link(CtxArg),
            Worker = #worker{
                ctx_id = CtxId,
                ctx_mod = CtxMod,
                pid = CtxPid,
                ref_count = 0,
                last_use = undefined
            },
            ets:insert(?WORKERS, Worker),
            {ok, CtxPid};
        [] ->
            case remove_worker() of
                true ->
                    acquire(CtxId, {CtxMod, CtxArg}, St);
                false ->
                    {error, all_workers_active}
            end
    end.


release(#client{} = Client) ->
    #client{
        ctx_id = CtxId,
        ref_count = RefCount
    } = Client,
    CtxRC = ets:update_counter(?WORKERS, CtxId, {#worker.ref_count, -1 * RefCount}),
    true = ets:delete_object(?CLIENTS, Client),

    % Blow up if we find an invalid ref count
    if CtxRC >= 0 -> ok; true ->
        erlang:error({invalid_context_ref_count, CtxId})
    end,

    if CtxRC /= 0 -> ok; true ->
        % RefCount dropped below zero so place this context
        % into the removable pool
        LastUse = erlang:monotonic_time(),
        ets:update_element(?WORKERS, CtxId, {#worker.last_use, LastUse}),
        ets:insert(?LRU, {LastUse, CtxId})
    end.


remove_worker() ->
    case ets:first(?LRU) of
        '$end_of_table' ->
            false;
        Timestamp ->
            [{_, CtxId}] = ets:lookup(?LRU, Timestamp),
            [Worker] = ets:lookup(?WORKERS, CtxId),
            #worker{
                ctx_mod = CtxMod,
                pid = CtxPid,
                ref_count = 0
            } = Worker,
            CtxMod:stop(CtxPid),
            ets:delete(?WORKERS, CtxId),
            ets:delete(?LRU, Timestamp),
            true
    end.


max_workers() ->
    config:get_integer("ateles", "max_workers", ?MAX_WORKERS).


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

    % Assert any ref count in ?WORKERS matches
    % the value found in ?CLIENTS
    ets:foldl(fun(Worker, _) ->
        #worker{
            ctx_id = CtxId,
            ref_count = RefCount
        } = Worker,
        true = RefCount >= 0,
        RefCount = maps:get(CtxId, RefCounts, 0)
    end, nil, ?WORKERS),

    % Assert any ref count discovered in ?CLIENTS
    % is accurate in ?WORKERS
    maps:fold(fun(CtxId, RefCount, _) ->
        [Worker] = ets:lookup(?WORKERS, CtxId),
        #worker{
            ref_count = RC
        } = Worker,
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

    % If a worker has a ref count of 0, it must have
    % a timestamp set in the LRU
    ets:foldl(fun(Worker, _) ->
        case Worker of
            #worker{ctx_id = CtxId, ref_count = 0, last_use = LU} ->
                [{LU, CtxId}] = ets:lookup(?LRU, LU);
            #worker{ref_count = N, last_use = undefined} when N > 0 ->
                ok
        end
    end, nil, ?WORKERS),

    % Assert every entry in the LRU has a ref count
    % of zero in ?WORKERS and ?CLIENTS. The check for
    % ?CLIENTS relies on our previously computed
    % RefCounts map.
    ets:foldl(fun({LU, CtxId}, _) ->
        [Worker] = ets:lookup(?WORKERS, CtxId),
        #worker{
            ref_count = 0,
            last_use = LU
        } = Worker,
        false = maps:is_key(CtxId, RefCounts)
    end, nil, ?LRU),

    ok.
-endif.
