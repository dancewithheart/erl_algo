-module(trie_set).
-export([new/0, add_element/2, is_element/2, from_list/1, starts_with/2]).

-export_type([emptyTrie/0, trieNode/1, trie/1]).
-record(trie, {val, map}).

-type map(K,V) :: #{K => V}.
-type emptyTrie() :: { trie, false, #{} }.
-type trieNode(K) :: { trie, boolean(), map(K, boolean()) }.
-type trie(K) :: emptyTrie() | trieNode(K).

%% @doc creates empty Trie
-spec( new() -> emptyTrie() ).
new() -> #trie{ val= false, map = maps:new()}.

%% @doc check if [K] is element of Trie
-spec( is_element([K], trie(K)) -> boolean() ).
is_element([],     #trie{val = X}) -> X;
is_element([K|KS], #trie{map = M}) ->
  case maps:find(K, M) of
    {ok, N} -> is_element(KS, N);
    _ -> false
  end.

%% @doc insert value (vales are lists, so they are a paths) into the Trie
-spec( add_element([K], trie(K)) -> trie(K) ).
add_element([], T) -> T#trie{val = true};
add_element([K|KS], T = #trie{map = M}) ->
  Next = maps:get(K, M, new()),
  Inserted = add_element(KS, Next),
  T#trie{map = maps:put(K, Inserted, M)}.

%% @doc converts list to Trie
-spec from_list(list(K)) -> trie(K).
from_list(T) -> lists:foldl(fun add_element/2, new(), T).

%% @doc check if given prefix exists in Trie
-spec starts_with(list(K), trie(K)) -> boolean().
starts_with([],     _) -> true;
starts_with([K|KS], #trie{map = M}) ->
  case maps:find(K, M) of
    {ok, N} -> starts_with(KS, N);
    _ -> false
  end.
