-module(fac).
-compile([export_all]).

fac(N) ->
    fac(1, 0, N).

fac(F, S, N) ->
    case N of
        0 -> S;
        _ -> fac(F + S, F, N - 1)
    end.

fac2(1) ->
    1;
fac2(0) ->
    0;
fac2(N) ->
    fac(N -2) + fac(N - 1).





