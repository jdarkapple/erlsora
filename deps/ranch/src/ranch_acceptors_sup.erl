-module(ranch_acceptors_sup).
-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).

-spec start_link(ranch:ref(), non_neg_integer(), module(), any()) -> { ok, pid() }.
start_link(Ref, NbAcceptors, Transport, TransOpts) ->
	supervisor:start_link(?MODULE, [Ref, NbAcceptors, Transport, TransOpts]).

init([Ref, NbAcceptors, Transport, TransOpts]) ->
	ConnsSup = ranch_server:get_connections_sup(Ref),
	LSocket = case proplists:get_value(socket, TransOpts) of
				  undefined -> 
					  {ok, Socket} = Transport:listen(TransOpts),
					  Socket;
				  Socket ->
					  Socket
			  end,
	{ok, {_, Port}} = Transport:sockname(LSocket),
	ranch_server:set_port(Ref, Port),
	Procs = [
			{{acceptor, self(), N}, {ranch_acceptor, start_link,
									 [LSocket, Transport, ConnsSup]},
									 permanent, brutal_kill, worker, []}
			|| N <- lists:seq(1, NbAcceptors)],
	{ok, {{one_for_one, 10, 10}, Procs}}.
