-module(my_random).
-compile([export_all]).

test() ->
    Seed = "0123",
    Width = 4,
    Count = 30,
    test(Seed, Count, Width).

test(Seed, Count, Width) ->
    NL = random_nums(Seed, Count, Width),
    io:format(" format NL: ~p ~n", [NL]),
    ok.

random_nums(Seed, Count, Width) ->
    case is_args_reasonable(Seed, Count, Width) of
        ok ->
            random_numbers(Seed, Count, Width);
        {error, _} = Reason ->
            Reason
    end.

is_args_reasonable(Seed, Count, Width) ->
    SeedLen = length(Seed),
    LimitWidth = get_limit_width(SeedLen, Count),
    is_args_reasonable(Width, LimitWidth).

is_args_reasonable(Width, LimitWidth) when Width >= LimitWidth ->
    ok;
is_args_reasonable(_Width, _LimitWidth) ->
    {error, overflow}.


get_limit_width(Num, Count) ->
    get_limit_width(Num, Count, 1).

get_limit_width(Num, Count, LimitWidth) when Count > Num ->
    get_limit_width(Num, Count/Num, LimitWidth + 1);
get_limit_width(_Num, _Count, LimitWidth) ->
    LimitWidth.

random_numbers(Seed, Count, Width) ->
    L = sub_random_nums(Count),
    format(Seed, Width, shuffle(L)).

sub_random_nums(Count) ->
    sub_random_numbers(Count, Count, []).

sub_random_numbers(Upper, Count, L) when Count > 0 ->
    KV = {get_random(Upper), Count - 1},
    sub_random_numbers(Upper, Count - 1, [KV | L]);
sub_random_numbers(_Upper, _Count, L) ->
        L.

get_random(Upper) when is_integer(Upper) ->
    rand(Upper);
get_random(_Upper) ->
    rand(1000000).

get_random() ->
    rand(1000000).

rand(Upper) ->
    {N1, N2, N3} = now(),
    No1 = N2 * get_no2(),
    No2 = N3 * get_no1(),
    No3 = N1 * get_no3(),
    {RandomNo, _} = random:uniform_s(Upper, {No1, No2, No3}),
    RandomNo - 1.

get_no1() ->
    list_to_integer(os:getpid()).

get_no2() ->
    {_, _, No3} = os:timestamp(),
    No3.

get_no3() ->
    Pids = os:cmd("ps -e"),
    sub_get_no3(Pids, length(Pids), 5, 0).

sub_get_no3(_, _, 0, No) -> No;
sub_get_no3(Pids, Len, Count, No) ->
    Location = random:uniform(get_no2()) rem Len + 1,
    NChar = lists:nth(Location, Pids),
    NNo = case is_integer(NChar) of
            true ->
                No + NChar;
            _ ->
                No + list_to_integer(NChar)
           end,
    sub_get_no3(Pids, Len, Count - 1, NNo).

shuffle(L) when is_list(L) ->
    {_, NL} = lists:unzip(lists:keysort(1, L)),
    NL.

format(Seed, Width, L) when is_list(L) andalso is_integer(Width) ->
    System = integer_to_list(length(Seed)),
    Wid = integer_to_list(Width),
    NL = [lists:flatten(io_lib:format("~" ++ Wid ++ "." ++ System ++ ".0X", [X, ""])) || X <- L].
