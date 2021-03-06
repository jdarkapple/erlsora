-module(ranch_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ranch_server = ets:new(ranch_server, [named_table, ordered_set, public]),
    {ok, { {one_for_one, 5, 10}, [{ranch_server, {ranch_server, start_link, []},
                                   permanent, 5000, worker, [ranch_server]}]} }.

