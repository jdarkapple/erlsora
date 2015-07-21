-module(ranch_conns_sup).

%% API
-export([start_link/6]).
-export([start_protocol/2]).
-export([active_connections/1]).

%% Supervisor internals
-export([init/7]).
-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-type conn_type() :: worker | supervisor.
-type shutdown() :: brutal_kill | timeout().

-record(state, {
         parent = undefined :: pid(),
         ref :: ranch:ref(),
         conn_type  :: conn_type(),
         shutdown :: shutdown(),
         transport = undefined :: module(),
         protocol = undefiend :: module(),
         opts :: any(),
         ack_timeout :: timeout(),
         max_conns = undefined :: ranch:max_conns()
         }).

-spec active_connections(pid()) -> non_neg_integer().
active_connections(SupPid) ->
    Tag = erlang:monitor(process, SupPid),
    catch erlang:send(SupPid, {?MODULE, active_connections, self(), Tag}, [noconnect]),
    receive
        {Tag, Ret} ->
            erlang:demoitor(Tag, [flush]),
            Ret;
        {'DOWN', Tag, _, _, noconnection} ->
            exit({nodedown, node(SupPid)});
        {'DOWN', Tag, _, _, Reason} ->
            exit(Reason)
    after 5000 ->
              erlang:demonitor(Tag, [flush]),
              exit(timeout)
    end. 


