-module(trie_set).
-export([new/0, add/2, member/2, from_list/1, starts_with/2]).

-type map(K,V) :: #{K => V}.
-type emptyTrie() :: {trie, false, #{} }.
-type trie(K) :: emptyTrie() | {trie, boolean(), map(K, boolean()) }.

-spec( new() -> emptyTrie() ).
new() -> {trie, false, maps:new()}.

%% @doc check if [K] is member of Trie
-spec( member([K], trie(K)) -> boolean() ).
member([],     {trie, X, _}) -> X;
member([K|KS], {trie, _, M}) ->
  case maps:find(K, M) of
    {ok, N} -> member(KS, N);
    _ -> false
  end.

%% @doc insert value (vales are lists, so they are a paths) into the Trie
-spec( add([K], trie(K)) -> trie(K) ).
add([], {trie, _, M}) -> {trie, true, M};
add([K|KS], {trie, V, M}) ->
  Next = maps:get(K, M, new()),
  Inserted = add(KS, Next),
  {trie, V, maps:put(K, Inserted, M)}.

%% @doc converts list to Trie
-spec from_list(list(K)) -> trie(K).
from_list(T) -> lists:foldl(fun add/2, new(), T).

starts_with([],     {trie, _, _}) -> true;
starts_with([K|KS], {trie, _, M}) ->
  case maps:find(K, M) of
    {ok, N} -> starts_with(KS, N);
    _ -> false
  end.
