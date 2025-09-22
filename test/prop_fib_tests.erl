-module(prop_fib_tests).
-import(fib, [fib/1]).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

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

prop_sum_two_previous() ->
  ?FORALL(N, non_neg_integer(),
    fib(N) + fib(N+1) == fib(N+2)
  ).

prop_eq_naive_impl() ->
  ?FORALL(N, non_neg_integer_below(30),
    fib(N rem 30) == fib_oracle(N rem 30)
  ).

% naive fibonacci O(2^n)
fib_oracle(0) -> 1;
fib_oracle(1) -> 1;
fib_oracle(N) when N > 1 -> fib_oracle(N-1) + fib_oracle(N-2).

non_neg_integer_below(N) ->
    ?LET(L, non_neg_integer(), L rem N).
