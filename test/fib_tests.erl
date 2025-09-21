-module(fib_tests).
-import(fib, [fibSlow/1]).
-include_lib("eunit/include/eunit.hrl").

fibSlow_test_() ->
  [
    ?_assert( fibSlow(0) =:= 1 ),
    ?_assert( fibSlow(1) =:= 1 ),
    ?_assert( fibSlow(2) =:= 2 ),
    ?_assertEqual( fibSlow(5), 8 ),
    ?_assertEqual( fibSlow(10), 89 ),
    ?_assertEqual( fibSlow(42), 165580141 )
  ].
