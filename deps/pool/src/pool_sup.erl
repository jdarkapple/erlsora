-module(pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
	ok.

start_link(Mod, Args) ->
    supervisor:start_link(?MODULE, [Mod, Args]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Mod, Args]) ->
    {ok, { {simple_one_for_one, 0, 1},
		   [{Mod, {Mod, start_link, [Args]},
			temporary, 5000, worker, [Mod]}]} }.
