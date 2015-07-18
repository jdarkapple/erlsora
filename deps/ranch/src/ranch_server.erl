-module(ranch_server).
-behaviour(gen_server).

%% API
-export([
		 start_link/0,
		 set_new_listener_opts/3,	
		 cleanup_listener_opts/1,
		 set_connections_sup/2,
		 get_connections_sup/1,
		 set_port/2,
		 get_port/1,
		 set_max_connections/2,
		 get_max_connections/1,
		 set_protocol_options/2,
		 set_protocol_options/1,
		 count_connections/1
		]).

%% gen_server
-export([
		 init/1,
		 handle_call/3,
		 handle_info/2,
		 handle_cast/2,
		 code_change/3,
		 terminate/2
		]).

-define(TAB, ?MODULE).

-type monitors() :: [{{reference(), pid()}, any()}].

-record(state, {
	 monitors = [] :: monitors()
	}).

%% API

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec set_new_listener_opts(ranch:ref(), ranch:max_conns(), any()) -> ok.
set_new_listener_opts(Ref, MaxConns, Opts) ->
	gen_server:call(?MODULE, {set_new_listener_opts, Ref, MaxConns, Opts}).

-spec cleanup_listener_opts(ranch:ref()) -> ok.
cleanup_listener_opts(Ref) ->
	_ = ets:delete(?TAB, {port, Ref}),
	_ = ets:delete(?TAB, {max_conns, Ref}),
	_ = ets:delete(?TAB, {opts, Ref}),
	ok.

-spec set_connections_sup(ranch:ref(), pid()) ->  true | false.
set_connections_sup(Ref, Pid) ->
	true = gen_server:call(?MODULE, {set_connections_sup, Ref, Pid}),
	ok.

-spec get_connections_sup(ranch:ref()) -> pid().
get_connections_sup(Ref) ->
	ets:lookup_element(?TAB, {conns_sup, Ref}, 2).

-spec set_port(ranch:ref(), inet:port_number()) -> ok.
set_port(Ref, Port) ->
	gen_server:call(?MODULE, {set_port, Ref, Port}).

-spec get_port(ranch:ref()) -> inet:port_number().
get_port(Ref) ->
	ets:lookup_element(?MODULE, {port, Ref}, 2).

-spec set_max_connections(ranch:ref(), ranch:max_conns()) -> ok.
set_max_connections(Ref, MaxConnections) ->
	gen_server:call(?MODULE, {set_max_conns, Ref, MaxConnections}).

-spec get_max_connections(ranch:ref()) -> ranch:max_conns().
get_max_connections(Ref) ->
	ets:lookup_element(?TAB, {max_conns, Ref}, 2).


%% gen_server callback
init() ->
	ok.

handle_call({set_new_listener_opts, Ref, MaxConns, Opts}, _From, State) ->
	ets:insert(?TAB, {{max_conns, Ref}, MaxConns}),
	ets:insert(?TAB, {opts, Ref}, Opts),
	{reply, ok, State};
handle_call({set_connections_sup, Ref, Pid}, _From, State) ->
	case ets:insert_new(?TAB, {{conns_sup, Ref}, Pid}) of
		true ->
			MonitorRef = erlang:monitor(process, Pid),
			{reply, true,
				State#state{monitors = [{{MonitorRef, Pid}, Ref} | MonitorRef]}};
		false ->
			{reply, false, State}
	end.
	
handle_call({set_port, Ref, Port}, _From, State) ->
	true = ets:insert(?TAB, {{port, Ref}, Port}),
	{reply, ok, State};
handle_call({set_max_conns, Ref, MaxConns}, _From, State) ->
	ets:insert(?TAB, {{max_conns, Ref}, MaxConns}),
	ConnsSup = get_connections_sup(Ref),
	ConnsSup ! {set_max_conns, MaxConns},
	{reply, ok, State};
