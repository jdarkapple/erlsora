-module(ranch).

-export([start_listener/6]).
-export([start_app/0]).
-export([start_test/0]).
-export([test/0]).

test() ->
    start_app(),
    start_test().

start_app() ->
    application:start(ranch).

start_test() ->
    start_test(ref1, [{port, 6666}, {reuseaddr, true}]).

start_test(Ref, Opts) ->
    start_listener(Ref, 10, ranch_tcp, Opts, handler, [{protoOpts, any}]).

start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtOpts) ->
    _ = code:ensure_loaded(Transport),
    case erlang:function_exported(Transport, name, 0) of
        true  ->
            Ret = supervisor:start_child(
                    ranch_sup, child_spec(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtOpts)),
            Ret;
        false ->
            exit(no_transport)
    end.

child_spec(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtOpts) ->
    {ranch_listener,
     {ranch_listener, start_link, [Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtOpts]},
    permanent, 5000, worker, [ranch_listener]}.

