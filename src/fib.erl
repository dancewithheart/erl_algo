-module(fib).
-export([fibSlow/1]).

-spec fibSlow(non_neg_integer()) -> pos_integer().
fibSlow(0) -> 1;
fibSlow(1) -> 1;
fibSlow(N) when N > 1 -> fibSlow(N-1) + fibSlow(N-2).
