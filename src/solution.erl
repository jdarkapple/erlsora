-module(solution).
%-export([main/0]).
%
%process_input(Repeats) ->
%	Line = io:get_line(""),
%	case string:len(Line) of
%		1 -> ok;
%		_ -> output(Line, Repeats),
%			 process_input(Repeats)
%	end.
%
%output(_, 0) -> ok;
%output(Line, Repeats) ->
%	io:format(Line),
%	output(Line, Repeats - 1).
%
%main() ->
%	{ok, [Repeats]} = io:fread("", "~d"),
%	process_input(Repeats).

-export([start/0]).

process_input(Repeat, Acc) ->
	case io:get_line("") of
		Done when Done == eof; Done == "\n" ->
			io:format("~s~n", [Done]),
			output(lists:reverse(Acc));
		Line ->
			Val = string:strip(Line, right, $\n),
			Str = lists:duplicate(Repeat, Val),
			process_input(Repeat, [Str | Acc])
	end.

output(Lines) ->
	io:format("~w", [Lines]),
	Out = [string:join(L, "\n") ++ "\n" || L <- Lines],
	io:format("~s", [Out]).

start() ->
	{ok, [Repeat]} = io:fread("", "~d"),
	io:format("~w~n", [Repeat]),
	process_input(Repeat, []).
