-module(utils).
-compile([export_all]).

-export([timer/2]).
-export([record_to_proplist/2]).
-export([new_paste_id/0]).
-export([pmap/2]).

record_to_proplist(Record, Fields) ->
    record_to_proplist(Record, Fields, '_record').

record_to_proplist(Record, Fields, TypeKey) 
    when tuple_size(Record) - 1 =:= length(Fields) ->
        lists:zip([TypeKey | Fields], tuple_to_list(Record)).

%%%----------------------------------------------------------

timer(Time, Fun) ->
    spawn(fun() ->
            receive 

            after Time ->
                ?MODULE:Fun()
            end
         end).

%%%----------------------------------------------------------

alarm() ->
    io:format(" ******** alarm ********~n").

%%%----------------------------------------------------------

floor(X) when X < 0 ->
    T = trunc(X),
    case (X - T) =:= 0of
        true -> T;
        false -> T -1
    end;
floor(X) ->
    trunc(X).

%%%----------------------------------------------------------

int_pow(_X, 0) ->
    1;
int_pow(X, N) when N > 0 ->
    int_pow(X, N, 1).

int_pow(X, N, R) when N < 2 ->
    R * X;
int_pow(X, N, R) ->
    int_pow(X * X, N bsr 1, case N band 1 of 1 -> R * X ; 0 -> R end).

%%%----------------------------------------------------------

fac(Num) ->
    fac(1, 1, Num).

fac(Collecter, Counter, MaxNum) when Counter > MaxNum->
       Collecter;
fac(Collecter, Counter, MaxNum) ->
    fac(Collecter * Counter, Counter + 1, MaxNum).
fac2(1) ->
    1;
fac2(Num) ->
    Num * fac2(Num - 1).

%将string类型的erlang数据转换程erlang类型
% "{aa, bb}" => {aa, bb}
string_to_term(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _ -> {}
            end;
        _ ->
            {}
    end.


% 获得进程状态(gen_server, gen_fsm) 
% 内部调用了sys:send_system_msg/2
write_state_to_file(Pid, FileName) ->
	Status = sys:get_status(Pid),
	file:write_file(FileName, io_lib:format("~p", [Status])).

%% 把{a, b, c} 作为字符串写入文件中
write_term_to_file() ->
	lists:flatten(io_lib:format("~p", [{a, b, c}])).

%% 清空当前shell
clear() ->
	io:format("\033[2J]").
%% 改变shell下输出位置
domove(X, Y) ->
	io:format("\e[~B;~BH]", [Y, X]).

cls() ->
	clear(),
	domove(1, 1).

%test_fun() ->
%	fun_to_str(fun() -> list_to_atom("hello world") end).

%% 控制台下可用
fun_to_str() ->
	{env, [Env]} = erlang:fun_info(fun() -> list_to_atom("xiix") end, env),
	Abs = erlang:element(size(Env), Env), 
	Str = erl_pp:expr({'fun', 1, {clauses, Abs}}),
	io:format([Str | "\n"]).

%% 输出当前正在执行的方法信息
who_am_i() ->
	{current_function, {M, F, A}} = process_info(self(), current_function),
	io:format("i am ~p:~p/~p~n", [M, F, A]).

who_am_i2() ->
	catch throw(away),
	[{Module, Fun, Arity, _} | _] = erlang:get_stacktrace(),
	io:format("I am ~p:~p/~p!~n", [Module, Fun, Arity]).

%% 记忆化例子
fact(0) -> 1;
fact(N) ->
	case erlang:get({'fact', N}) of
		F when is_integer(F) ->
			F;
		'undefined' ->
			F = N * fact(N - 1),
			erlang:put({'fact', N}, F),
			F
	end.

%% 
process_infos() ->
	filelib:ensure_dir("./log/"),
	File = "./log/processes_infos.log",
	{ok, Fd} = file:open(File, [write, raw, binary, append]),
	Fun = fun(Pi) ->
				  Info = io_lib:format("=>~p \n\n", [Pi]),
				  case filelib:is_file(File) of
					  true -> file:write(Fd, Info);
					  false ->
						  file:close(Fd),
						  {ok, NewFd} = file:open(File, [write, raw, binary, append]),
						  file:wirte(NewFd, Info)
				  end,
				  timer:sleep(20)
		  end,
	[Fun(erlang:process_info(P)) || P <- erlang:processes()],
	file:close(Fd).
	
new_paste_id() ->
	Initial = random:uniform(62) -1,
	new_paste_id(<<Initial>>, 7).
new_paste_id(Bin, 0) ->
	Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"    >>,
	<< <<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin >>;
new_paste_id(Bin, Rem) ->
	Next = random:uniform(62) - 1,
	new_paste_id(<<Bin/binary, Next>>, Rem - 1).

pmap(Fun, Args) ->
	Parent = self(),
	[receive {Pid, Result} -> Result end 
	 || Pid <- [spawn(fun() ->
					Parent ! {self(), Fun(Arg)} 
				end) || Arg <- Args]].

-spec pid_to_tuple(Pid :: pid()) -> tuple().
pid_to_tuple(Pid) ->
    List = erlang:pid_to_list(Pid),
    [[$< | H], M, T] = string:tokens(List, "."),
    {list_to_integer(H), list_to_integer(M), list_to_integer(lists:droplast(T))}.

get_statename(Pid) ->
    hd([Name || L=[_|_] <- element(4, sys:get_status(Pid)),
                Data=[_|_] <- [proplists:get_value(data, L)],
                Name <- [proplists:get_value("StateName", Data, 0)],
                is_atom(Name)]).

wait_until(Fun, _, 0) -> error({timeout, Fun});
wait_until(Fun, Interval, Tries) ->
    case Fun() of
        true -> ok;
        false ->
            timer:sleep(Interval),
            wait_until(Fun, Interval, Tries - 1)
    end.

ascii_tolower(B) ->
    iolist_to_binary(ascii_tolower_s(binary_to_list(B))).

ascii_tolower_s([C | Cs]) when C >= $A , C =< $Z ->
    [C + ($a - $A) | ascii_tolower_s(Cs)];
ascii_tolower_s([C | Cs]) ->
    [C | ascii_tolower_s(Cs)];
ascii_tolower_s([]) ->
    [].

test_run_time() ->
    erlang:statistics(wall_clock),
    lists:seq(1, 1000) ++ lists:seq(1, 1000),
    T = {_, Time} = erlang:statistics(wall_clock),
    io:format("~p~n", [T]).
    
