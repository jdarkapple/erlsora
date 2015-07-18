-file("test_define.erl", 1).

test() ->
    no_heheaa.

module_info() ->
    erlang:get_module_info(test_define).

module_info(X) ->
    erlang:get_module_info(test_define, X).

