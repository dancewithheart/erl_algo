-module(fib_tests).
-import(fib, [fib/1]).
-include_lib("eunit/include/eunit.hrl").

fib_test_() ->
  [
    ?_assert( fib(0) =:= 1 ),
    ?_assert( fib(1) =:= 1 ),
    ?_assert( fib(2) =:= 2 ),
    ?_assertEqual( fib(5), 8 ),
    ?_assertEqual( fib(10), 89 ),
    ?_assertEqual( fib(42), 165580141 )
  ].
