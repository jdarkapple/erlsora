-module(my_proc).
-export([start_link/0]).
-export([init/1]).

start_link() ->
	proc_lib:start_link(my_proc, init, [self()]).

init(Parent) ->
	io:format("**** init ****"),
	proc_lib:init_ack({ok, hehe}).
