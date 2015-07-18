-module(sort).
-compile([export_all]).

qsort([]) -> [];
qsort([Pivot | T]) ->
    qsort([ X || X <- T, X < Pivot])
    ++ [Pivot] ++
    qsort([ X || X <- T, X > Pivot]).

qsort2([]) -> [];
qsort2([Pivot | Rest]) ->
    {Smaller, Bigger} = split(Pivot, Rest),
    lists:append(qsort2(Smaller), [Pivot | qsort2(Bigger)]).

split(Pivot, Rest) ->
    split(Pivot, Rest, [], []).

split(_, [], Smaller, Bigger) ->
    {Smaller, Bigger};
split(Pivot, [H | T], Smaller, Bigger) when H < Pivot ->
    split(Pivot, T, [H | Smaller], Bigger);
split(Pivot, [H | T], Smaller, Bigger) when H >= Pivot ->
    split(Pivot, T, Smaller, [H | Bigger]).
