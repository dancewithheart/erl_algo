-module(fib).
-export([fib/1]).

%% @docs compute n-th Fibonacci number
%% O(n) complexity, uses bottom up dynamic programming and remembers only previous 2 numbers
%% @see https://www.geeksforgeeks.org/dsa/program-for-nth-fibonacci-number/
-spec fib(non_neg_integer()) -> pos_integer().
fib(0) -> 1;
fib(1) -> 1;
fib(N) when N > 1 -> fibGo(N, 2, 1, 1).

% N-2 -> N-1
% N-1 -> N-1 + N-2
fibGo(N, I, N_1, N_2) when I =< N ->
  fibGo(N, I+1, N_1 + N_2, N_1);
fibGo(_, _, N_1, _) -> N_1.
