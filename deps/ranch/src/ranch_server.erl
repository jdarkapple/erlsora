-module(ranch_server).

-behaviour(gen_server).

%% API functions
-export([start_link/0]).

-export([set_new_listener/3]).
-export([set_connection_sup/2,
         get_connection_sup/1]).
-export([set_port/2,
         get_port/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {monitors = []}).

-define(TAB, ranch_server).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_new_listener(Ref, MaxConns, Opts) ->
    gen_server:call(?MODULE, {set_new_listener, Ref, MaxConns, Opts}).

set_connection_sup(Ref, ConnsPid) ->
    gen_server:call(?MODULE, {set_connection_sup, Ref, ConnsPid}).

get_connection_sup(Ref) ->
    ets:lookup_element(?TAB, {conns, Ref}, 2).

set_port(Ref, Port) ->
    gen_server:call(?MODULE, {set_port, Ref, Port}).

get_port(Ref) ->
    ets:lookup_element(?TAB, {port, Ref}, 2).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({set_new_listener, Ref, MaxConns, Opts}, _From, State) ->
    io:format("set_new_listener"),
    ets:insert(?TAB, {{max_conns, Ref}, MaxConns}),
    ets:insert(?TAB, {{opts, Ref}, Opts}),
    {reply, ok, State};
handle_call({set_connection_sup, Ref, ConnsPid}, _From, State) ->
    io:format("set_connection_sup ~n", []),
    ets:insert(?TAB, {{conns, Ref}, ConnsPid}),
    {reply, ok, State};
handle_call({set_port, Ref, Port}, _From, State) ->
    ets:insert(?TAB, {{port, Ref}, Port}),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
