-module(ranch_acceptor).

-export([start_link/3]).
-export([loop/3]).


-spec start_link(inet:socekt(), module(), pid()) -> {ok, pid()}.
start_link(LSocket, Transport, ConnsSup) ->
	Pid = spawn_link(?MODULE, loop, [LSocket, Transport, ConnsSup]),
	{ok, Pid}.

-spec loop(inet:socket(), module(), pid()) -> no_return().
loop(LSocket, Transport, ConnsSup) ->
	_ = case Transport:accept(LSocket, infinity) of
			{ok, CSocket} ->
				Transport:controlling_process(CSocket, ConnsSup),

				ranch_conns_sup:start_protocol(ConnsSup, CSocket);
			{error, emfile} ->
				receive after 100 -> ok end;
			{error, Reason} when Reason =/= closed ->
				ok
		end,
	flush(),
	?MODULE:loop(LSocket, Transport, ConnsSup).

flush() ->
	receive Msg ->
				error_logger:error_msg(
				  "Ranch acceptor receive unexpected message: ~p~n",
				  [Msg]),
				flush()
	after 0 ->
			  ok
	end.
