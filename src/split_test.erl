-module(split_test).
-export([split/0]).

split() ->
    split_string("name;foo1;foo2").

split_string(L) ->
    L1 = lists:reverse(L),
    sub_split_string([], L1, []).

sub_split_string([], [], Result) -> Result;
sub_split_string(R1, [], Result) -> [R1 | Result];
sub_split_string(R1, [H | T], Result) ->
    case H of
        59 ->
            case R1 of
                [] -> sub_split_string([], T, Result);
                _ -> sub_split_string([], T, [R1 | Result])
            end;
        _ -> sub_split_string([H | R1], T, Result)
    end.
