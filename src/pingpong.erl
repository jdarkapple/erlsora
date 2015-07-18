-module(pingpong).

-export([start/0, ping/2, pong/0]).

ping(0, Pong_Pid) ->
    Pong_Pid ! finished,
    io:format("ping finished ~n", []);
ping(N, Pong_Pid) ->
    Pong_Pid ! {ping, self()},
    receive
        pong ->
            io:format("Ping receive pong ~n", [])
    end,
    ping(N - 1, Pong_Pid).

pong() ->
    receive 
        finished ->
            io:format("pong finished ~n", []);
        {ping, Ping_Pid} ->
            io:format("Pong receive ping ~n", []),
            Ping_Pid ! pong,
            pong()
    end.

start() ->
    Pong_Pid = spawn(pingpong, pong, []),
    spawn(pingpong, ping, [5, Pong_Pid]),
    io:format("Main process Exit ~n", []).
