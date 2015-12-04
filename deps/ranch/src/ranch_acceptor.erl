-module(ranch_acceptor).

-export([start_link/4]).
-export([loop/4]).

start_link(Ref, Transport, ConnSup, LSocket) ->
    io:format("***** acceptor start ~n"),
    Pid = spawn_link(?MODULE, loop, [Ref, Transport, ConnSup, LSocket]),
    {ok, Pid}.

loop(Ref, Transport, ConnSup, LSocket) ->
    case Transport:accept(LSocket, infinity) of
        {ok, Socket} ->
            Transport:controlling_process(Socket, ConnsSup),
            ranch_conns_sup:start_protocol(Socket),
            io:format("**** get a connect by:~p", [self()]);
        _ ->
            error
    end, 
    loop(Ref, Transport, ConnSup, LSocket).
