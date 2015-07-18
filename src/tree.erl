-module(tree).

-export([lookup/2, insert/3, write_tree/1]).

lookup(Key, nil) ->
    not_found;
lookup(Key, {Key, Value, _, _}) ->
    {found, Value};
lookup(Key, {Key1, _, Smaller, _}) when Key < Key1 ->
    lookup(Key, Smaller);
lookup(Key, {Key1, _, _, Bigger}) when Key > Key1 ->
    lookup(Key, Bigger).

insert(Key, Value, nil) ->
    {Key, Value, nil, nil};
insert(Key, Value, {Key, _, Smaller, Bigger}) ->
    {Key, Value, Smaller, Bigger};
insert(Key, Value, {Key1, Value1, Smaller, Bigger}) when Key < Key1 ->
    {Key1, Value1, insert(Key, Value, Smaller), Bigger};
insert(Key, Value, {Key1, Value1, Smaller, Bigger}) when Key > Key1 ->
    {Key1, Value1, Smaller, insert(Key, Value, Bigger)}.

write_tree(T) ->
    write_tree(0, T).

write_tree(D, nil) ->
    tab(D),
    io:format('nil', []);
write_tree(D, {Key, Value, Smaller, Bigger}) ->
    D1 = D + 6,
    write_tree(D1, Bigger),
    io:format("~n", []),
    tab(D),
    io:format("~w ==> ~w~n", [Key, Value]),
    write_tree(D1, Smaller).

tab(0) ->
    ok;
tab(N) ->
    io:format("----", []),
    tab(N - 1).
