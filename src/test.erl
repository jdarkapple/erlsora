-module(test).

-export([test/0]).
-export([test_chinese/0]).
-export([rec/0]).

high() ->
    process_flag(priority, high),
    high(30).
high(0) ->
    io:format("HHH~n");
high(N) ->
    io:format("HHH~n"),
    high(N - 1).

low() ->
    process_flag(priority, low),
    low(30).
low(0) ->
    io:format("L~n");
low(N) ->
    io:format("L~n"),
    low(N - 1).

test() ->
    spawn(fun() -> high() end),
    spawn(fun() -> low() end).

test_chinese() ->
    io:format("谢谢").

test_vim()->
	hehe.

rec() ->
    rec().
