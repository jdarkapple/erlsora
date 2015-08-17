%%%--------------------------------------------------------------------
%%% Author        : zm
%%% Email         : xinxizhaomin@163.com
%%% Last modified : 2015-08-17 16:37
%%% FileName      : pool_worker.erl
%%% Description   :
%%%--------------------------------------------------------------------
-module(pool_worder).

-callback start_link(WorkerArgs) -> {ok, Pid} |
									{error, {already_started, Pid}} |
									{error, Reason} when
	  WorkerArgs :: proplists:proplist(),
	  Pid		 :: pid(),
	  Reason	 :: term().

