-module(ranch).

-export([
		 
		]).

-type max_conns() :: non_neg_integer() | infinity.
-export_type([max_conns/0]).

-type ref() :: any().
-export_type([ref/0]).
