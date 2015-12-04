-module(ranch_listener).

-behaviour(supervisor).

%% API functions
-export([start_link/6]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtOpts) ->
    supervisor:start_link(?MODULE, [Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtOpts]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtOpts]) ->
    MaxConns = proplists:get_value(max_connections, TransOpts, 1000),
    ok = ranch_server:set_new_listener(Ref, MaxConns, ProtOpts),
    Children = [{ranch_conns_sup,
                 {ranch_conns_sup, start_link, [Ref, NbAcceptors, Transport, MaxConns, Protocol, ProtOpts]},
                permanent, 5000, worker, [ranch_conns_sup]},
                {ranch_acceptors_sup,
                 {ranch_acceptors_sup, start_link, [Ref, NbAcceptors, Transport, TransOpts]},
                permanent, 5000, worker, [ranch_acceptors_sup]}],
    {ok, {{rest_for_one, 5, 10}, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
