-module(fib_tests).
-import(fib, [fib/1]).
-include_lib("eunit/include/eunit.hrl").

-spec fibSlow(non_neg_integer()) -> pos_integer().
fibSlow(0) -> 1;
fibSlow(1) -> 1;
fibSlow(N) when N > 1 -> fibSlow(N-1) + fibSlow(N-2).

fibSlow_test_() ->
  [
    ?_assert( fibSlow(0) =:= 1 ),
    ?_assert( fibSlow(1) =:= 1 ),
    ?_assertEqual( 2,         fibSlow(2) ),
    ?_assertEqual( 3,         fibSlow(3) ),
    ?_assertEqual( 5,         fibSlow(4) ),
    ?_assertEqual( 8,         fibSlow(5) ),
    ?_assertEqual( 89,        fibSlow(10) )
    % , ?_assertEqual( 433494437, fibSlow(42) )
  ].

fib_test_() ->
  [
    ?_assert( fib(0) =:= 1 ),
    ?_assert( fib(1) =:= 1 ),
    ?_assertEqual( 2,         fib(2) ),
    ?_assertEqual( 3,         fib(3) ),
    ?_assertEqual( 5,         fib(4) ),
    ?_assertEqual( 8,         fib(5) ),
    ?_assertEqual( 89,        fib(10) ),
    ?_assertEqual( 433494437, fib(42) )
  ].
