%%%-------------------------------------------------------------------
%% @doc ateles public API
%% @end
%%%-------------------------------------------------------------------

-module(ateles_app).

-behaviour(application).

-export([start/2, stop/1]).

-compile(export_all).
-compile(nowarn_export_all).

start(_StartType, _StartArgs) ->
    ateles_rpc:init(),
    timer:apply_after(1000, ?MODULE, testing, []),
    ateles_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
testing() ->
    Out = ateles_rpc:create_context(#{id => <<"foo">>}),
    io:format("RES ~p ~n", [Out]).
