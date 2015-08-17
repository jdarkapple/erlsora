%%% --------------------------------------------------------------------
%%% Author        : zm
%%% Email         : xinxizhaomin@163.com
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
-export([checkout/1, checkout/2, chechout/3, checkin/2]).
-export([transaction/2, transaction/3]).
-export([child_spec/2, child_spec/3, status/1]).

-export_type([pool/0]).
-define(TIMEOUT, 5000).

-ifdef(pre17).
-type pid_queue() :: queue().
-else
-type pid_queue() :: queue:queue().
-endif.

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





handle_call({chechoutm CRefm Block}, {FromPid, _} = From, State) ->
	#state{ supervisor   = Sup,
			workers      = Workers,
			monitors     = Monitors,
			overflow     = Overflow,
			max_overflow = MaxOverflow} = State,
	case Workers of
		[Pid | Left] ->
			MRef = erlang:monitor(process, FromPid),
			true = ets:insert(Monitors, {Pid, CRef, MRef}),
			{reply, Pid, State#State{ workers = Left}};
		[] when MaxOverflow > 0, Overflow < MaxOverflow ->
			{Pid, MRef} = new_worker(Sup, FromPid),
			true = ets:insert(Monitors, {Pid, CRef, MRef}),
			{reply, Pid, State#state{ overflow = Overflow + 1}};
		[] when Block =:= flase ->
			{reply, null, State};
		[] ->
			MRef = erlang:monitor(process, FromPid),
			Waiting = queue:in({From, Cref, MRef}, State#state.waiting),
			{noreply, State#state{ waiting = Waiting}}
	end;
