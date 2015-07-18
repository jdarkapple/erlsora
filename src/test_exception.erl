-module(test_exception).

-compile([export_all]).

test() ->
    try f() of
        Val ->
            Val
    catch
        Class:Error ->
            io:format("~p:~p~n", [Class, Error])
    end,
    hehe.

f() ->
    exit("byebyte").

test2(F1, F2) when (is_function(F1, 0) andalso is_function(F2, 0)) ->
    try F1()
    catch 
        throw:X ->
            {{caught, throw, X}, F2()};
        exit:X ->
            {{caught, exit, X}, F2()};
        error:X ->
            {{caught, error, X}, F2()}
    end.


