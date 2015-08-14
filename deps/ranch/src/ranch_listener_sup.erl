-module(ranch_listener_sup).
-behaviour(supervisor).

-export([start_link/6]).
-export([init/1]).

-spec start_link(ranch:ref(), non_neg_integer(), module(), any(), module(), any())
    -> {ok, pid()}.
start_link(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts) ->
    MaxConns = proplists:get_value(max_connections, TransOpts, 1024),
    ranch_server:set_new_listener_opts(Ref, MaxConns, ProtoOpts),
    supervisor:start_link(?MODULE, {
        Ref, NbAcceptors, Transport, TransOpts, Protocol}).

init({Ref, NbAcceptors, Transport, TransOpts, Protocol}) ->
    AckTimeout = proplists:get_value(ack_timeout, TransOpts, 5000),
    ConnType = proplists:get_value(connection_type, TransOpts, worker),
    Shutdown = proplists:get_value(shutdown , TransOpts, 5000),
    ChildSpecs = [
     {ranch_conns_sup, {ranch_conns_sup, start_link, 
        [Ref, ConnType, Shutdown, Transport, AckTimeout, Protocol]},
      permanent, infinity, supervisor, [ranch_conns_sup]},
      {ranch_acceptors_sup, {ranch_acceptors_sup, start_link,
         [Ref, NbAcceptors, Transport, TransOpts]},
      permanent, infinity, supervisor, [ranch_acceptors_sup]}
     ],
    {ok, {{rest_for_one, 10, 10}, ChildSpecs}}.
