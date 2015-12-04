-module(ranch_conns_sup).

-export([start_link/6]).
-export([init/7]).
-export([start_protocol/1]).

start_link(Ref, NbAcceptors, Transport, MaxConns, Protocol, ProtOpts) ->
    proc_lib:start_link(?MODULE, init,
                        [self(), Ref, NbAcceptors, Transport, MaxConns, Protocol, ProtOpts]).

init(Parent, Ref, NbAcceptors, Transport, MaxConns, Protocol, ProtOpts) ->
    io:format("~p init ~n", [?MODULE]),
    proc_lib:init_ack(Parent, {ok, self()}),
    ranch_server:set_connection_sup(Ref, self()),
    loop().

loop() ->
    receive
        Msg ->
            Msg
    end.

start_protocol(Socket) ->
    ConnsSup = ranch_server:get_connection_sup(Ref),
    ConnsSup ! {start_protocol, self(), Socket},
    receive
        ok -> ok
    end.
