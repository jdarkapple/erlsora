%%%--------------------------------------------------------------------
%%% Author        : 
%%% Email         : 
%%% Last modified : 2015-08-17 16:37
%%% FileName      : pool_worker.erl
%%% Description   :
%%%--------------------------------------------------------------------
-module(pool_worker).

-callback start_link(WorkerArgs :: proplists:proplist()) -> {ok, Pid :: pid()} |
									{error, {already_started, Pid :: pid()}} |
									{error, Reason :: term()}.
