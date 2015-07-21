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

-spec init(pid(), ranch:ref(), conn_type(), shutdown(),
    module(), timeout(), module()) -> no_return().
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
                            Curconns2 = CurConns + 1,
                            if CurConns2 < MaxConns ->
                                   To ! self(),
                                   loop(State, Curconns2, NbChildren + 1, Sleepers);
                               true ->
                                   loop(State, CurrConns2, NbChildren, [To | Sleepers])
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
