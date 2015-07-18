-module(commFixed).

-export([init/0, rcv/1, snd/2, sync/1]).

-define(N1, 'n1@192.168.1.111').
-define(N2, 'n1@192.168.1.111').

init() ->
	Rcv = spawn(?N2, ?MODULE, rcv, [none]),
	spawn(?N1, ?MODULE, snd, [Rcv, 1]).


rcv(N) ->
	receive
		{sync, Snd} ->
			Snd ! {sync, N},
			N1 = N;
		X ->
			io:format("got ~p~n", [X]),
			N1 = X
	end,
	rcv(N1).

snd(Rcv, N) ->
	erlang:monitor(process, Rcv),
	snd_(Rcv, N).

snd_(Rcv, N) ->
	receive
		{'DOWN', _, _, _, noconnection} ->
			N = sync(Rcv)
	after 1000 ->
		ok
	end,
	Rcv ! N,
	snd_(Rcv, N + 1).

sync(Rcv) ->
	erlang:monitor(process, Rcv),
	Rcv ! {sync, self()},
	receive
		{'DOWN', _, _, _, noconnection} ->
			timer:sleep(5000),
			sync(Rcv);
		{sync, N} ->
			snd_(Rcv, N + 1)
	end.
