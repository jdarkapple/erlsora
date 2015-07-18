-module(looper).
-compile([export_all]).

loop() ->
    receive 
        abc -> 
            io:format("received abc ###~n", []),
            loop();
        stop ->
            stop
    end.

loop2() ->
    receive
        abc ->
            io:format("received abc ###~n", []),
            loop2();
        stop ->
            stop
    after 15000 ->
        receive
            Any ->
                io:format("any = ~p~n", [Any])
        end,
        io:format("clear~n"),
        loop2()
    end.

loop3() ->
    receive 
        aaa ->
            io:format("aaa~n"),
            timer:sleep(15000),
            io:format("sleep after receive abc done.~n"),
            loop3();
        stop ->
            stop
    after 13000 ->
            io:format("time out~n")
    end.
