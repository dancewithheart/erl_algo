-module(prop_fib).
-import(fib, [fib/1]).
-include_lib("proper/include/proper.hrl").

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
