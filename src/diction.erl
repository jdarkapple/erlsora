%%字典demo1
-module(diction).
-export([new/0, lookup/2, add/3, delete/2]).
-export([test/3]).
new() ->
    [].

lookup(Key, [{Key, Value} | _Rest]) ->
    {value, Value};
lookup(Key, [_Pair | Rest]) ->
    lookup(Key, Rest);
lookup(_Key, []) ->
    undefined.

add(Key, Value, Diction) ->
    [{Key, Value} | delete(Key, Diction)].

delete(Key, [{Key, _Value} | Rest]) ->
    Rest;
delete(Key, [_Pair | Rest]) ->
    delete(Key, Rest);
delete(_Key, []) ->
    [].


test(JID, Nick, Dict) ->
    dict:update(Nick,
        fun(Entry) ->
            case lists:member(JID, Entry) of
                true -> Entry;
                false -> [JID | Entry]
            end
        end,
        [JID], Dict).
