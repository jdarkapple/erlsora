-module(simple_web_server).
-export([start/1]).

start(Port) ->
	spawn_link(fun() ->
				  {ok, Socket} = gen_tcp:listen(Port, [{active, true}]),
				  loop(Socket)
		  end).

loop(Socket) ->
	{ok, Conn} = gen_tcp:accept(Socket),
	io:format("accpeted a connection"),
	Handler = spawn(fun() -> handle(Conn) end),
	gen_tcp:controlling_process(Conn, Handler),
	loop(Socket).

handle(Conn) ->
	Data = gen_tcp:recv(Conn, 0),
	io:format("received data: ~p", [Data]),
	gen_tcp:send(Conn, response("Hello Wrold!!!")).

response(Str) ->
	Bin = iolist_to_binary(Str),
	iolist_to_binary(
	 io_lib:fwrite(
		"HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
	  [size(Bin), Bin])).
