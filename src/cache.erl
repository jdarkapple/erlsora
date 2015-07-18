-module(cache).

-behaviour(gen_server).

%% API functions
-export([start_link/0]).

-export([set/2, set/3, get/1, del/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-define(SERVER, ?MODULE).

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

set(K, V, T) ->
        gen_server:call(?SERVER, {set, K, V, T}).

set(K, V) ->
        gen_server:call(?SERVER, {set, K, V}).

get(K) ->
        gen_server:call(?SERVER, {get, K}). 

del(K) ->
    gen_server:call(?SERVER, {del, K}).
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
    {ok, gb_trees:empty()}.

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
handle_call({set, K, V, T}, _From, Tree) ->
    case gb_trees:lookup(K, Tree) of
        none -> 
            none;
        {value, {V, forever}} ->
            none;
        {value, {V, OldTRef}} ->
            timer:cancel(OldTRef)
    end,
    {ok, TRef} = timer:apply_after(T * 1000, ?MODULE, del, [K]),
    Tree2 = gb_trees:enter(K, {V, TRef}, Tree), 
    {reply, {ok, K, V}, Tree2};

handle_call({set, K, V}, _From, Tree) ->
    case gb_trees:lookup(K, Tree) of
        none ->
            none;
        {value, {V, forever}} ->
            none;
        {value, {V, OldTRef}} ->
            timer:cancel(OldTRef)
    end,
    Tree2 = gb_trees:enter(K, {V, forever}, Tree),
    {reply, {ok, K, V}, Tree2};
handle_call({get, K}, _From, Tree) ->
    Reply = case gb_trees:lookup(K, Tree) of
                none ->
                    none;
                {value, {Value, _}} ->
                    {ok, Value}
            end,
    {reply, Reply, Tree};
handle_call({del, K}, _From, Tree) ->
    Tree2 = gb_trees:delete_any(K, Tree),
    io:format("K = ~p has be deleted ~n", [K]),
    {reply, {ok, K}, Tree2};

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
