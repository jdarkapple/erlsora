-module(test_define).
-compile([export_all]).

-ifdef('HEHE').
-define(NAME, is_hehe).
-else.
-define(NAME, no_heheaa).
-endif.

test() ->
	?NAME.
