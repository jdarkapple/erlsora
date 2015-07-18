-module(sqrt).
-compile([export_all]).

sqrt(Num) ->
    do_sqrt(1.0, Num).

do_sqrt(Guess, Num) ->
    case good_enough(Guess, Num) of
        true -> Guess;
        _ -> do_sqrt(improve_guess(Guess, Num), Num)
    end.
            
good_enough(Guess, Num) ->
    abs(square(Guess) - Num) < 0.001.

improve_guess(Guess, Num) ->
    (Guess + Num / Guess) / 2.

average(X, Y) ->
    (X + Y) / 2.
square(X) ->
    X * X.
