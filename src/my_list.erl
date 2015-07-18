-module(my_list).
-compile([export_all]).
-export([drop_last/2,
         test_list_sort/0,
         index_of/2]).

drop_last(N, List) ->
    {ToDrop, List2} = lists:foldr(fun(E, {0, Acc}) ->
        {0, [E | Acc]};
        (_, {ToDrop, Acc}) ->
            {ToDrop -1, Acc}
        end, {N, []}, List),
    {N - ToDrop, List2}.

test_list_sort() ->
    L = [{a, 3}, {c, 5}, {d, 4}, {b, 1}],
    lists:keysort(2, L).

index_of(Item, List) -> 
    index_of(Item, List, 1).

index_of(_, [], _) ->
    not_found;
index_of(Item, [Item | _], N) ->
    N;
index_of(Item, [_Head | Res], N) ->
    index_of(Item, Res, N + 1).

%% test with append element from head and tail

add_list_head(N) ->
    add_list_head(N, []).
add_list_head(0, L) ->
    lists:reverse(L),
    ok;
add_list_head(N, L) ->
    add_list_head(N - 1, [N | L]).

add_list_tail(N) ->
    add_list_tail(N, []).
add_list_tail(0, L) ->
    ok;
add_list_tail(N, L) ->
    add_list_tail(N - 1, L ++ [N]).


add(L, Init) ->
	lists:foldl(fun do_add/2, Init, L).
do_add(N, Init) ->
	N + Init.


list_to_map(List) ->
	list_to_map(List, maps:new()).

list_to_map([], Map) ->
	Map;
list_to_map([K, V | Rest], Map) ->
	list_to_map(Rest, maps:put(K, V, Map)).
