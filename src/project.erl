-module(project).

-export([q1/1]).

%% if we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
%% Find the sum of all the multiples of 3 or 5 below 1000.
q1( N ) ->
    do_q1( N - 1, 0 ).

do_q1( 0, Sum ) ->
    Sum;
do_q1( N, Sum ) when (N rem 3 == 0) or (N rem 5 == 0) ->
    do_q1( N - 1, Sum + N );
do_q1( N, Sum ) ->
    do_q1( N - 1, Sum).
