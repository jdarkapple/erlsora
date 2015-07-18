-module(mem_sup).
-compile([export_all]).
-define(BUFFER_SIZE, 1024).

get_memory_info() -> get_memory_info(os:type()).

get_memory_info({unix, linux}) ->
    File = read_proc_file("/proc/meminfo"),
    Lines = string:tokens(File, "\n"),
    Dict = dict:from_list(lists:map(fun parse_line_linux/1, Lines)),
    MemTotal = dict:fetch('MemTotal', Dict),
    MemFree = dict:fetch('MemFree', Dict),
    SwapTotal = dict:fetch('SwapTotal', Dict),
    SwapFree = dict:fetch('SwapFree', Dict),
    Inactive = dict:fetch('Inactive', Dict),
    PhysicalMemorySize = MemTotal,
    VirtualMemorySize = MemTotal + SwapTotal,
    AvailablePhysicalMemorySize = MemFree + Inactive,
    AvailableVirtualMemorySize = MemFree + Inactive + SwapFree,
    {ok, {PhysicalMemorySize, VirtualMemorySize, AvailablePhysicalMemorySize
         , AvailableVirtualMemorySize}}.

%% ["MemTotal   443434 kB"]
parse_line_linux(Line) ->
    [Name, RHS | _REST] = string:tokens(Line, ":"),
    [Value | UnitsRest] = string:tokens(RHS, " "),
    Value1 = case UnitsRest of  
                [] -> list_to_integer(Value);%% 没有单位
                ["kB"] -> list_to_integer(Value) * 1024
             end,
    {list_to_atom(Name), Value1}.
%% /proc/meminfo文件的大小一直是0，file:read_file方法不能读取其中的内容
read_proc_file(File) ->
    {ok, IoDevice} = file:open(File, [read, raw]),
    Res = read_proc_file(IoDevice, []),
    file:close(IoDevice),
    lists:flatten(lists:reverse(Res)).

read_proc_file(IoDevice, Acc) ->
    case file:read(IoDevice, ?BUFFER_SIZE) of
        {ok, Res} -> read_proc_file(IoDevice, [Res | Acc]);
        eof -> Acc
    end.

%% erlang自带的内存监控模块 （需要先开启sasl下的os_mon模块）
%% application:start(sasl).
%% application:start(os_mon).
%%
%% L = memsup:get_system_memory_data(),
%%      Total = proplists:get_value(total_memory, L),
%%      Freed = proplists:get_value(free_memory, L),
%%      Cached = proplists:get_value(cached_memory, L),
%%      {ok, (Total - Freed - Cached) * 100 div Total}.
