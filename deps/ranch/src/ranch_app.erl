-module(ranch_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([profile_output/0]).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	_ = consider_profiling(),
    ranch_sup:start_link().

stop(_State) ->
    ok.

-spec profile_output() -> ok.
profile_output() ->
	eprof:stop_profiling(),
	eprof:log("procs.profile"),
	eprof:analyze(procs),
	eprof:log("total.profile"),
	eprof:analyze(total).

consider_profiling() ->
	case application:get_env(profile) of
		{ok, true} ->
			io:format("profile true"),
			{ok, _Pid} = eprof:start(),
			eprof:start_profiling([self()]);
		_ ->
			io:format("no profile "),
			not_profiling
	end.
