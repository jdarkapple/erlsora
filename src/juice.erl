-module(juice).
-export([make_many_juice/0]).

make_juice(apple) ->
	apple_juice;
make_juice(orange) ->
	orange_juice;
make_juice(pear) ->
	pear_juice;
make_juice(banana) ->
	0 / 0.

pmap(Fun, List) ->
	Pid = self(),
	Pids = lists:map(fun(I) ->
							 spawn(fun() -> execute(Pid, Fun, I) end)
					 end, List),
	Result = gather(Pids, []),
	lists:reverse(Result).

execute(Recv, Fun, I) ->
	Recv ! {self(), Fun(I)}.

gather([], Result) -> Result;
gather([Pid | Rest], Result) ->
	receive
		{Pid, Data} -> 
			gather(Rest, [Data | Result])
	end.

gather2([]) ->
	[];
gather2([H | T]) ->
	receive
		{H, Ret} ->
			[Ret | gather2(T)]
	end.

make_many_juice() ->
	Fruits = [apple, orange, pear, banana],
	pmap(fun(F) ->
				 try
					 make_juice(F)
				 catch _:_ ->
						   fail
				 end end, Fruits).
