%%gen_fsm状态机例子
-module(npc).
 
-behaviour(gen_fsm).
 
%% API
-export([start_link/0]).
 
%% gen_fsm callbacks
-export([init/1, static/2, moving/2, handle_event/3,
     handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
 
-export([hero_join/0, hero_leave/0]).
 
-define(SERVER, ?MODULE).
 
-record(npc, {state}).
 
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).
 
%% 初始化NPC为静止状态
init([]) ->
    io:format("init...~n"),
    State = #npc{state = static},
    io:format("init State: ~p~n", [State]),
    {ok, static, State}. 
%% 英雄进入视野
hero_join() ->
    gen_fsm:send_event(?SERVER, hero_join).
 
%% 英雄离开视野
hero_leave() ->
    gen_fsm:send_event(?SERVER, hero_leave).
 
%% 静止状态下，接受来自客户端的事件
static(Event, State) ->
    case Event of
    hero_join -> %% 英雄进入视野
        do_moving(), %% 执行动作
        NewState = State#npc{state = moving},
        io:format("npc set state: ~p~n", [NewState]),
        {next_state, moving, NewState}
    end.
 
%% 移动状态下，接受来自客户端的事件
moving(Event, State) ->
    case Event of
    hero_leave -> %% 英雄离开视野
        do_static(), %% 执行动作
        NewState = State#npc{state = static},
        io:format("npc set state: ~p~n", [NewState]),
        {next_state, static, NewState}
    end.
 
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.
 
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.
 
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
 
terminate(_Reason, _StateName, _State) ->
    ok.
 
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
 
%% NPC 开始移动，进入移动状态
do_moving() ->
    io:format("npc beigin moving...~n").
 
%% NPC 停止移动，进入静止状态
do_static() ->
    io:format("npc stop moving, join static...~n").
