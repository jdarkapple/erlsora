-module(ranch_acceptors_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/4]).

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
start_link(Ref, NbAcceptors, Transport, TransOpts) ->
    supervisor:start_link(?MODULE, [Ref, NbAcceptors, Transport, TransOpts]).

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
init([Ref, NbAcceptors, Transport, TransOpts]) ->
    ConnSup = ranch_server:get_connection_sup(Ref),
    LSocket = case proplists:get_value(socket, TransOpts) of
                  undefined ->
                      {ok, Socket} = Transport:listen(TransOpts),
                      Socket;
                  Socket ->
                      Socket
              end,
    {ok, {_, Port}} = Transport:sockname(LSocket),
    ranch_server:set_port(Ref, Port),
    io:format("begin to start acceptor~n"),
    Acceptors = [{{acceptor, N, self()}, {ranch_acceptor, start_link, [Ref, Transport, ConnSup, LSocket]},
                 permanent, brutal_kill, worker, []}
                || N <- lists:seq(1, NbAcceptors)],
    {ok, {{one_for_one, 5, 10}, Acceptors}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
