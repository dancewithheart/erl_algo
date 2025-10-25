-module(prop_trie_set).
-include_lib("proper/include/proper.hrl").
-import(trie_set, [member/2, new/0, add/2, starts_with/2, from_list/1]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

% member k empty = false
prop_member_empty_trie_is_false() ->
  ?FORALL( K, list(term()),
    member(K, new()) == false
  ).

% member k (add k) = true
prop_member_k_add_k_is_true() ->
  ?FORALL( {K, T}, {string(), trie_set(string())},
    member(K, add(K, T)) == true
  ).

% if k1 /= k2 => member k1 (add k2 t) = member k1 t
prop_add_different_value_does_not_affect_member() ->
  ?FORALL( {T, K1, K2},
    {trie_set(string()), string(), string()},
    ?IMPLIES( K1 =/= K2,
      member(K1, add(K2, T)) == member(K1, T)
    )).

% starts_with k1 (add k1 ++ k2 t)
prop_starts_with_substring_added() ->
  ?FORALL( {T, K1, K2},
    {trie_set(string()), string(), string()},
    starts_with(K1, add(K1 ++ K2, T)) == true
  ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
trie_set(Gen) -> ?LET(L, list(Gen), from_list(L)).
