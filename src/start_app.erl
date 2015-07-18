-module(start_app).

start_applications(Apps) ->
	manage_appliation(fun lists:foldl/3,
					  fun application:start/1,
					  fun application:stop/1,
					  already_started,
					  cannot_start_application,
					  Apps).

stop_applications(Apps) ->
	manage_appliation(fun lists:foldr/3,
					  fun application:stop/1,
					  fun application:start/1,
					  not_started,
					  cannot_stop_application,
					  Apps).

manage_appliation(Iteration, Do, UnDo, SkipError, ErrorTag, Apps) ->
	Iteration(fun(App, Acc) ->
					  case Do(App) of
						  ok -> [App | Acc];
						  {error, {SkipError, _}} -> Acc;
						  {error, Reason} ->
							  lists:foreach(UnDo, Acc),
							  throw({error, {ErrorTag, App, Reason}})
					  end
			  end, [], Apps).

%%------------------------------------------------------------------------

start1(App) ->
	case application:start1(App) of
		ok ->
			ok;
		{error, {not_started, Dep}} ->
			start1(Dep),
			start1(App)
	end.

%%-------------------------------------------------------------------------

start2() ->
	start2(lager).

start2(App) ->
	start_ok(App, application:start(App, permanent)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
	ok = start2(Dep),
	start2(App);
start_ok(App, {error, Reason}) ->
	erlang:error({app_start_failed, App, Reason}).
