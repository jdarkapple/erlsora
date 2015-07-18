-module(mochiweb_util).
-compile([export_all]).


record_to_proplist(Record, Fields) ->
    record_to_proplist(Record, Fields, '_record').

record_to_proplist(Record, Fields, TypeKey) 
    when tuple_size(Record) - 1 =:= length(Fields) ->
        lists:zip([TypeKey | Fields], tuple_to_list(Record)).

