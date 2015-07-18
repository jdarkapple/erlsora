-module(record_test).

-record(x, {name, zz}).
-record(y, {yy, name}).

-record(man, {name, age = 0, school}).

-compile([export_all]).
-export([test1/0, test2/0]).
-export([test3/0, test4/0]).
-define(create(Type, Name), #Type{name = Name}).
-define(VALUE(Call),io:format("~p = ~p ~n", [??Call, Call])).
-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE, ?LINE, X])).
-else.
-define(LOG(X), true).
-endif.

test1() -> ?create(x, "Noel"). % -> {x, "Noel", undefined}
test2() -> ?create(y, "Noel"). % -> {y, undefined, "Noel"}

test3() ->
    M2 = #man{name = 'zm', age = 23, school = "NO.14"},
    M3 = M2#man{name = "tt", age = 24},
    io:format("M3 is : ~p ~n", [M3]).

show(#man{name = Name, age = Age} = M)when is_record(M, man) ->
    io:format("Name : ~p Age : ~p ~n", [Name, Age]).

test4() ->
    ?VALUE(length([1 ,2, 3])).

test5() ->
    ?LOG(hello).
