-module(ranch_tcp).

-export([name/0]).
-export([accept/2]).
-export([listen/1]).
-export([peername/1]).
-export([sockname/1]).

name() ->
    gen_tcp.

accept(Socket, Timeout) ->
    gen_tcp:accept(Socket, Timeout).

listen(Opts) ->
    io:format("****** ~p~n", [Opts]),
    gen_tcp:listen(0, Opts).

peername(Socket) ->
    inet:peername(Socket).

sockname(Socket) ->
    inet:sockname(Socket).
