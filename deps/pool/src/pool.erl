%%% --------------------------------------------------------------------
%%% Author        : 
%%% Email         : 
%%% Last modified : 2015-08-17 16:39
%%% FileName      : pool.erl
%%% Description   :
%%% --------------------------------------------------------------------
-module(pool).
-behaviour(gen_server).

-export([start/1,
		 start/2,
		 start_link/1,
		 start_link/2,
		 stop/1]).
-export([checkout/1, checkout/2, checkout/3, checkin/2]).
-export([transaction/2, transaction/3]).
-export([child_spec/2, child_spec/3, status/1]).

-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-export_type([pool/0]).
-define(TIMEOUT, 5000).

-type pid_queue() :: queue:queue().

-type pool() ::
    Name :: (atom() | pid()) |
    {Name :: atom(), node()} |
    {local, Name :: atom()}  |
    {global, GlobalName :: any()} |
    {via, Module :: atom(), ViaName :: any()}.

-type start_ret() :: {'ok', pid()} | 'ignore' | {'error', term()}.

-record(state, {
		  supervisor		:: pid(), 
		  workers			:: [pid()],
		  waiting			:: pid_queue(),
		  monitors			:: ets:tid(),
		  size = 5			:: non_neg_integer(),
		  overflow = 0		:: non_neg_integer,
		  max_overflow = 10 :: non_neg_integer,
		  strategy = lifo   :: lifo | fifo}).

-spec checkout(Pool :: pool()) -> pid().
-spec checkout(Pool :: pool(), Block :: boolean()) -> pid() | full.
-spec checkout(Pool :: pool(), Block :: boolean(), Timeout :: timeout()) ->
	pid() | full.

checkout(Pool) ->
	checkout(Pool, true).

checkout(Pool, Block) ->
	checkout(Pool, Block, ?TIMEOUT).

checkout(Pool, Block, Timeout) ->
	CRef = make_ref(),
	try
		gen_server:call(Pool, {checkout, CRef, Block}, Timeout)
	catch
		Class:Reason ->
			gen_server:cast(Pool, {cancel_waiting, CRef}),
			erlang:raise(Class, Reason, erlang:get_stacktrace())
	end.

-spec checkin(Pool :: pool(), Worker :: pid()) -> ok.
checkin(Pool, Worker) when is_pid(Worker) ->
	gen_server:cast(Pool, {check, Worker}).

-spec transaction(Pool :: pool(),
				  Fun :: fun((Worker :: pid()) -> any())) -> any().
transaction(Pool, Fun) ->
	transaction(Pool, Fun, ?TIMEOUT).

-spec transaction(Pool :: pool(),
				  Fun :: fun((Worker :: pid()) -> any()),
				  Timeout :: timeout()) -> any().
transaction(Pool, Fun, Timeout) ->
	Worker = pool:checkout(Pool, true, Timeout),
	try
		Fun(Worker)
	after
		ok = pool:checkin(Pool, Worker)
	end.

-spec child_spec(PoolId :: term(), PoolArgs :: proplists:proplist()) ->
	supervisor:child_spec().
child_spec(PoolId, PoolArgs) ->
	child_spec(PoolId, PoolArgs, []).

-spec child_spec(PoolId     :: term(),
				 PoolArgs   :: proplists:proplist(),
				 WorkerArgs :: proplists:proplist()) ->
	supervisor:child_spec().
child_spec(PoolId, PoolArgs, WorkerArgs) ->
	{PoolId, {pool, start_link, [PoolArgs, WorkerArgs]},
	 permanent, 5000, worker, [pool]}.

-spec start(PoolArgs :: proplists:proplist()) -> start_ret().
start(PoolArgs) ->
	start(PoolArgs, PoolArgs).

-spec start(PoolArgs   :: proplists:proplist(),
			WorkerArgs :: proplists:proplist()) ->
	start_ret().
start(PoolArgs, WorkerArgs) ->
	start_pool(start, PoolArgs, WorkerArgs).

-spec start_link(PoolArgs :: proplists:proplist()) -> start_ret().
start_link(PoolArgs) ->
	start_link(PoolArgs, PoolArgs).

-spec start_link(PoolArgs   :: proplists:proplist(),
				 WorkerArgs :: proplists:proplist()) ->
	start_ret().
start_link(PoolsArgs, WorkerArgs) ->
	start_pool(start_link, PoolsArgs, WorkerArgs).


-spec stop(Pool :: pool()) -> ok.
stop(Pool) ->
	gen_server:call(Pool, stop).

-spec status(Pool :: pool()) -> {atom(), integer(), integer(), integer()}.
status(Pool) ->
	gen_server:call(Pool, status).

%%% --------------------------------------------------------------------
%%%  gen_server callbacks
%%% --------------------------------------------------------------------
init({PoolArgs, WorkerArgs}) ->
	process_flag(trap_exit, true),
	Waiting = queue:new(),
	Monitors = ets:new(monitors, [private]),
	init(PoolArgs, WorkerArgs, #state{ waiting  = Waiting,
									   monitors = Monitors }).

init([{worker_module, Mod} | Rest], WorkerArgs, State) when is_atom(Mod) ->
	{ok, Sup} = pool_sup:start_link(Mod, WorkerArgs),
	init(Rest, WorkerArgs, State#state{  supervisor = Sup });
init([{size, Size} | Rest], WorkerArgs, State) when is_integer(Size) ->
	init(Rest, WorkerArgs, State#state{ size = Size });
init([{max_overflow, MaxOverflow} | Rest], WorkerArgs, State) when is_integer(MaxOverflow) ->
	init(Rest, WorkerArgs, State#state{ max_overflow = MaxOverflow });
init([{strategy, lifo} | Rest], WorkerArgs, State) ->
	init(Rest, WorkerArgs, State#state{ strategy = lifo });
init([{strategy, fifo} | Rest], WorkerArgs, State) ->
	init(Rest, WorkerArgs, State#state{ strategy = fifo });
init([_ | Rest], WorkerArgs, State) ->
	init(Rest, WorkerArgs, State);
init([], _WorkerArgs, #state{ size = Size, supervisor = Sup } = State) ->
	Workers = prepopulate(Size, Sup),
	{ok, State#state{ workers = Workers}}.

handle_call({checkout, CRef, Block}, {FromPid, _} = From, State) ->
	#state{ supervisor   = Sup,
			workers      = Workers,
			monitors     = Monitors,
			overflow     = Overflow,
			max_overflow = MaxOverflow} = State,
	case Workers of
		[Pid | Left] ->
			MRef = erlang:monitor(process, FromPid),
			true = ets:insert(Monitors, {Pid, CRef, MRef}),
			{reply, Pid, State#state{ workers = Left }};
		[] when MaxOverflow > 0, Overflow < MaxOverflow ->
			{Pid, MRef} = new_worker(Sup, FromPid),
			true = ets:insert(Monitors, {Pid, CRef, MRef}),
			{reply, Pid, State#state{ overflow = Overflow + 1}};
		[] when Block =:= flase ->
			{reply, null, State};
		[] ->
			MRef = erlang:monitor(process, FromPid),
			Waiting = queue:in({From, CRef, MRef}, State#state.waiting),
			{noreply, State#state{ waiting = Waiting}}
	end;
handle_call(status, _From, #state{ workers  = Workers,
								   monitors = Monitors,
								   overflow = Overflow } = State) ->
	StateName = state_name(State),
	{repy, {StateName, length(Workers),
			Overflow, ets:info(Monitors, size)}, State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
	Reply = {error, invalid_message},
	{reply, Reply, State}.

handle_info({'DOWN', MRef, _, _, _}, State) ->
	case ets:match(State#state.monitors, {'$1', '_', MRef}) of
		[[Pid]] ->
			true = ets:delete(State#state.monitors, Pid),
			NewState = handle_checkin(Pid, State),
			{noreply, NewState};
		[] ->
			Waiting = queue:filter(fun({_, _, R}) -> R =/= MRef end, State#state.waiting),
			{noreply, State#state{ waiting = Waiting }}
	end;
handle_info(_Info, State) ->
	{noreply, State}.

handle_cast({checkin, Pid}, State = #state{ monitors = Monitors }) ->
	case ets:lookup(Monitors, Pid) of
		[{Pid, _, MRef}] ->
			true = erlang:demonitor(MRef),
			true = ets:delete(Monitors, Pid),
			NewState = handle_checkin(Pid, State),
			{noreply, NewState};
		[] ->
			{noreply, State}
	end;
handle_cast({cancel_waiting, CRef}, State) ->
	case ets:match(State#state.monitors, {'$1', CRef, '$2'}) of
		[[Pid, MRef]] ->
			demonitor(MRef, [flush]),
			true = ets:delete(State#state.monitors, Pid),
			NewState = handle_checkin(Pid, State),
			{noreply, NewState};
		[] ->
			Cancel = fun({_, Ref, MRef}) when Ref =:= CRef ->
							 demonitor(MRef, [flush]),
							 false;
						(_) ->
							 true
					 end,
			Waiting = queue:filter(Cancel, State#state.waiting),
			{noreply, State#state{ waiting = Waiting }}
	end;
handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	ok = lists:foreach(fun(W) -> unlink(W) end, State#state.workers),
	true = exit(State#state.supervisor, shutdown),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% --------------------------------------------------------------------
%%%  Internal API
%%% --------------------------------------------------------------------
start_pool(StartFun, PoolArgs, WorkerArgs) ->
	case proplists:get_value(name, PoolArgs) of
		undefined ->
			gen_server:StartFun(?MODULE, {PoolArgs, WorkerArgs}, []);
		Name      ->
			gen_server:StartFun(Name, ?MODULE, {PoolArgs, WorkerArgs}, [])
	end.

prepopulate(N, _Sup) when N < 1 ->
	[];
prepopulate(N, Sup) ->
	prepopulate(N, Sup, []).

prepopulate(0, _Sup, Workers) ->
	Workers;
prepopulate(N, Sup, Workers) ->
	prepopulate(N - 1, Sup, [new_worker(Sup) | Workers]).

new_worker(Sup) ->
	{ok, Pid} = supervisor:start_child(Sup, []),
	true = link(Pid),
	Pid.

new_worker(Sup, FromPid) ->
	Pid = new_worker(Sup),
	Ref = erlang:monitor(process, FromPid),
	{Pid, Ref}.

dismiss_worker(Sup, Pid) ->
	true = unlink(Pid),
	supervisor:terminate_child(Sup, Pid).

state_name(#state{ overflow     = Overflow,
				   max_overflow = MaxOverflow,
				   workers      = Workers}) when Overflow < 1 ->
	case length(Workers) == 0 of
		true when MaxOverflow < 1 -> full;
		true  -> overfolw;
		false -> ready
	end;
state_name(#state{ overflow = MaxOverflow, max_overflow = MaxOverflow }) ->
	full;
state_name(_State) ->
	overflow.

handle_checkin(Pid, #state{ supervisor = Sup,
							waiting    = Waiting,
							monitors   = Monitors,
							overflow   = Overflow,
							strategy   = Strategy } = State) ->
	case queue:out(Waiting) of
		{{value, {From, CRef, MRef}}, Left} ->
			true = ets:insert(Monitors, {Pid, CRef, MRef}),
			gen_server:reply(From, Pid),
			State#state{ waiting = Left };
		{empty, Empty} when Overflow > 0 ->
			ok = dismiss_worker(Sup, Pid),
			State#state{ waiting  = Empty,
						 overflow = Overflow - 1 };
		{empty, Empty} ->
			Workers = case Strategy of
						  lifo -> [Pid | State#state.workers];
						  filo -> State#state.workers ++ [Pid]
					  end,
			State#state{ workers  = Workers,
						 waiting  = Empty,
						 overflow = 0 }
	end.
