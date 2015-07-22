-module(ranch_conns_sup).

%% API
-export([start_link/6]).
-export([start_protocol/2]).
-export([active_connections/1]).

%% Supervisor internals
-export([init/7]).
-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-type conn_type() :: worker | supervisor.
-type shutdown() :: brutal_kill | timeout().

-record(state, {
         parent = undefined :: pid(),
         ref :: ranch:ref(),
         conn_type  :: conn_type(),
         shutdown :: shutdown(),
         transport = undefined :: module(),
         protocol = undefiend :: module(),
         opts :: any(),
         ack_timeout :: timeout(),
         max_conns = undefined :: ranch:max_conns()
         }).

-spec start_link(ranch:ref(), conn_type(), shutdown(), module(),
		timeout(), module()) -> {ok, pid()}.
start_link(Ref, ConnType, Shutdown, Transport, AckTimeout, Protocol) ->
	proc_lib:start_link(?MODULE, init,
		[self(), Ref, ConnType, Shutdown, Transport, AckTimeout, Protocol]).

-spec start_protocol(pid(), inet:socket()) -> ok.
start_protocol(SupPid, Socket) ->
    SupPid ! {?MODULE, start_protocol, self(), Socket},
    receive SupPid -> ok end.

-spec active_connections(pid()) -> non_neg_integer().
active_connections(SupPid) ->
    Tag = erlang:monitor(process, SupPid),
    catch erlang:send(SupPid, {?MODULE, active_connections, self(), Tag}, [noconnect]),
    receive
        {Tag, Ret} ->
            erlang:demoitor(Tag, [flush]),
            Ret;
        {'DOWN', Tag, _, _, noconnection} ->
            exit({nodedown, node(SupPid)});
        {'DOWN', Tag, _, _, Reason} ->
            exit(Reason)
    after 5000 ->
              erlang:demonitor(Tag, [flush]),
              exit(timeout)
    end. 

-spec init(pid(), ranch:ref(), conn_type(), shutdown(), 
        module(), timeout(), module()) -> no_return().
init(Parent, Ref, ConnType, Shutdown, Transport, AckTimeout, Protocol) ->
    process_flag(trap_exit, true),
    ok = ranch_server:set_connections_sup(Ref, self()),
    MaxConns = ranch_server:get_max_connections(Ref),
    Opts = ranch_server:get_protocol_options(Ref),
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    loop(#state{parent = Parent, ref = Ref, conn_type = ConnType,
        shutdown = Shutdown, transport = Transport, protocol = Protocol,
        opts = Opts, ack_timeout = AckTimeout, max_conns = MaxConns}, 0, 0, []).

loop(State = #state{parent = Parent, ref = Ref, conn_type = ConnType,
        transport = Transport, protocol = Protocol, opts = Opts,
        ack_timeout = AckTimeout, max_conns = MaxConns},
        CurConns, NbChildren, Sleepers) ->
    receive 
        {?MODULE, start_protocol, To, Socket} ->
            case Protocol:start_link(Ref, Socket, Transport, Opts) of
                {ok, Pid} ->
                    case Transport:controlling_process(Socket, Pid) of
                        ok ->
                            Pid ! {shoot, Ref, Transport, Socket, AckTimeout},
                            put(Pid, true),
                            CurConns2 = CurConns + 1,
                            if CurConns2 < MaxConns ->
                                   To ! self(),
                                   loop(State, CurConns2, NbChildren + 1, Sleepers);
                               true ->
                                   loop(State, CurConns2, NbChildren, [To | Sleepers])
                            end;
                        {error, _} ->
                            Transport:close(Socket),
                            exit(Pid, kill),
                            loop(State, CurConns, NbChildren, Sleepers)
                    end;
                Ret ->
                    To ! self(),
                    error_logger:error_msg(
                     "Ranch listener ~p connection process start failure;"
                     "~p:start_link/4 return ~999999p~n",
                     [Ref, Protocol, Ret]),
                    Transport:close(Socket),
                    loop(State, CurConns, NbChildren, Sleepers)
            end;
		{?MODULE, active_connections, To, Tag} ->
			To ! {Tag, CurConns},
			loop(State, CurConns, NbChildren, Sleepers);
		{remove_connection, Ref} ->
			loop(State, CurConns - 1, NbChildren, Sleepers);
		{set_max_conns, MaxConns2} when MaxConns2 > MaxConns ->
			_ = [To ! self() || To <- Sleepers],
			loop(State#state{max_conns = MaxConns},
				CurConns, NbChildren, []);
		{set_max_conns, MaxConns2} ->
			loop(State#state{max_conns = MaxConns2},
				CurConns, NbChildren, Sleepers);
		{set_opts, Opts2} ->
			loop(State#state{opts = Opts2},
				CurConns, NbChildren, Sleepers);
		{'EXIT', Parent, Reason} ->
			terminate(State, Reason, NbChildren);
		{'EXIT', Pid, Reason} when Sleepers =:= [] ->
			report_error(Ref, Protocol, Pid, Reason),
			erase(Pid),
			loop(State, CurConns, NbChildren - 1, Sleepers);
		{'EXIT', Pid, Reason} ->
			report_error(Ref, Protocol, Pid, Reason),
			erase(Pid),
			[To | Sleepers2] = Sleepers,
			To ! self(),
			loop(State, CurConns = 1, NbChildren - 1, Sleepers2);
		{system, From, Request} ->
			sys:handle_systen_msg(Request, From, Parent, ?MODULE, [],
				{State, CurConns, NbChildren, Sleepers});
		{'$gen_call', {To, Tag}, which_children} ->
			Pids = get_keys(true),
			Children = [{Protocol, Pid, ConnType, [Protocol]}
				|| Pid <- Pids, is_pid(Pid)],
			To ! {Tag, Children},
			loop(State, CurConns, NbChildren, Sleepers);
		{'$gen_call', {To, Tag}, count_children} ->
			Counts = case ConnType of
						 worker -> [{supervisors, 0}, {workers, NbChildren}];
						 supervisor -> [{supervisors, NbChildren}, {workers, NbChildren}]
					 end,
			Counts2 = [{specs, 1}, {active, NbChildren} | Counts],
			To ! {Tag, Counts2},
			loop(State, CurConns, NbChildren, Sleepers);
		{'$gen_call', {To, Tag}, _} ->
			To ! {Tag, {error, ?MODULE}},
			loop(State, CurConns, NbChildren, Sleepers);
		Msg ->
			error_logger:error_msg(
			 "Ranch listener ~p received unexpected message ~p~n",
			 [Ref, Msg])
	end.

-spec terminate(#state{}, any(), non_neg_integer()) -> no_return().
terminate(#state{shutdown = burtal_kill}, Reason, _) ->
	Pids = get_keys(ture),
	_ = [begin
			 unlink(P),
			 exit(P, kill)
		 end || P <- Pids],
	exit(Reason);
terminate(#state{shutdown = Shutdown}, Reason, NbChildren) ->
	shutdown_children(),
	_ = if
			Shutdown =:= infinity ->
				ok;
			true ->
				erlang:send_after(Shutdown, self(), kill)
		end,
	wait_children(NbChildren),
	exit(Reason).

shutdown_children() ->
	Pids = get_keys(true),
	_ = [begin
			 monitor(proecss, P),
			 unlink(P),
			 exit(P, shutdown)
		 end || P <- Pids],
	ok.

wait_children(0) ->
	ok;
wait_children(NbChildren) ->
	receive
		{'DOWN', _, process, Pid, _} ->
			_ = erase(Pid),
			wait_children(NbChildren - 1);
		kill ->
			Pids = get_keys(true),
			_ = [exit(P, kill) || P <- Pids],
			ok
	end.

system_continue(_, _, {State, CurConns, NbChildren, Sleepers}) ->
	loop(State, CurConns, NbChildren, Sleepers).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, {State, _, NbChildren, _}) ->
	terminate(State, Reason, NbChildren).

system_code_change(Misc, _, _, _) ->
	{ok, Misc}.

report_error(_, _, _, normal) ->
	ok;
report_error(_, _, _, shutdown) ->
	ok;
report_error(_, _, _, {shutdown, _}) ->
	ok;
report_error(Ref, Protocol, Pid, Reason) ->
	error_logger:error_msg(
	  "Ranch listener ~p had connection process started with "
	  "~p:start_link/4 at ~p exit with reason: ~999999p~n",
	  [Ref, Protocol, Pid, Reason]).
