-module(ranch_tcp).
-behaviour(ranch_transport).

-export([name/0]).
-export([secure/0]).
-export([messages/0]).
-export([listen/1]).
-export([accept/2]).
-export([accept_ack/2]).

-export([connect/3]).
-export([connect/4]).
-export([recv/3]).
-export([send/2]).
-export([sendfile/2]).
-export([sendfile/4]).
-export([sendfile/5]).

-export([setopts/2]).
-export([controlling_process/2]).
-export([peername/1]).
-export([socketname/1]).
-export([shutdown/2]).
-export([close/1]).

-type opts() :: [{backlog, non_neg_integer()}
				| {ip, net:ip_address()}
				| {linger, {boolean(), non_neg_integer()}}
				| {nodelay, boolean()}
				| {port, inet:port_number()}
				| {raw, non_neg_integer(), non_neg_integer(),
				    non_neg_integer() | binary()}
				| {send_timeout, timeout()}
				| {send_timeout_close, boolean()}].
-export_type([opts/0]).

-spec name() -> atom().
name() -> tcp.

-spec secure() -> boolean().
secure() ->
	false.

messages() -> {tcp, tcp_closed, tcp_error}.

-spec listen(opts()) -> {ok, inet:socket()} | {error, atom()}.
listen(Opts) ->
	Opts2 = ranch:set_option_default(Opts, backlog, 1024),
	Opts3 = ranch:set_option_default(Opts2, send_timeout, 30000),
	Opts4 = ranch:set_option_default(Opts3, send_timeout_close, true),

	gen_tcp:listen(0, ranch:filter_options(Opts4,
	  [backlog, ip, linger, nodelay, port, raw,
	   send_timeout, send_timeout_close],
	  [binary, {active, false}, {packet, raw},
	   {reuseaddr, true}, {nodelay, true}])).

-spec accept(inet:socket(), timeout())
	-> {ok, inet:socket()} | {error, close | timeout | atom()}.
accept(LSocket, Timeout) ->
	gen_tcp:accept(LSocket, Timeout).

-spec accept_ack(inet:socket(), timeout()) -> ok.
accept_ack(_, _) ->
	ok.

-spec connect(inet:ip_address() | inet:hostname(),
			 inet:port_number(), any())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts) when is_integer(Port) ->
	gen_tcp:connect(Host ,Port,
				    Opts ++ [binary, {active, false}, {packet, raw}]).

-spec connect(inet:ip_address() | inet:hostname(),
			  inet:port_number(), any(), timeout())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts, Timeout) when is_integer(Port) ->
	gen_tcp:connect(Host, Port,
				    Opts ++ [binary, {active, false}, {packet, raw}],
				    Timeout).

-spec recv(inet:socket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	gen_tcp:recv(Socket, Length, Timeout).

-spec send(inet:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	gen_tcp:send(Socket, Packet).

