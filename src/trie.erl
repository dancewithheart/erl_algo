-module(trie).
-export([get/2, new/0, put/2, put/3, from_list/1]).

% -define(NANI, false).
% ne is a shortcut in Japanese for なにもない /nanimonai/ nothing
% relevant context: https://www.youtube.com/watch?v=dNQs_Bef_V8
% お前 は もう 死んでいる /Omae wa mou shinde iru/
% なに /Nani/
-define(NANI, ne).

% Scheme chooses #f (boolean false) to indicate lack of value, this is a problem when you return boolean
% Haskell and Scala uses special value (Nothing/None) and wrap value in (Just/Some),
% Erlang often uses: {ok, V} | error
% option(A) :: ?NONE | A
% has a nice benefit (or drawback) that option(option(A)) == option(A)
-type map(K,V) :: #{K => V}.
-type option(A) :: ?NANI | A.
-type emptyTrie() :: {trie, ?NANI, #{} }.
-type trie(K,X) :: emptyTrie() | {trie, option(X), map(K,X) }.

-spec( new() -> emptyTrie() ).
new() -> {trie, ?NANI, maps:new()}.

%% @doc get value bound to K in Trie
-spec( get([K], trie(K,V)) -> option(V) ).
get([], {trie, X,     _}) -> X;
get([K|KS], {trie, _, M}) ->
  case maps:find(K, M) of
    {ok, N} -> get(KS, N);
    _ -> ?NANI
  end.

%% @doc insert binding key := value into the Trie
-spec( put({[K], V}, trie(K,V)) -> trie(K,V) ).
put({Ks,X}, T) -> put(Ks, X, T).

%% @doc insert binding key := value into the Trie
-spec( put([K], V, trie(K,V)) -> trie(K,V) ).
put([], X, {trie, _, M}) -> {trie, X, M};
put([K|KS], X, {trie, V, M}) ->
  Next = maps:get(K, M, new()),
  Inserted = put(KS,X,Next),
  {trie, V, maps:put(K,Inserted,M)}.

%% @doc converts list to Trie
-spec from_list(list({K,V})) -> trie(K,V).
from_list(T) -> lists:foldl(fun put/2, new(), T).
