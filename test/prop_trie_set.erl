-module(prop_trie_set).
-include_lib("proper/include/proper.hrl").
-import(trie_set, [
  is_element/2, new/0, add_element/2,
  starts_with/2,
  from_list/1]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

% is_element k empty = false
prop_is_element_empty_trie_is_false() ->
  ?FORALL( K, list(term()),
    is_element(K, new()) == false
  ).

% is_element k (add_element k) = true
prop_is_element_k_add_k_is_true() ->
  ?FORALL( {K, T}, {string(), trie_set(string())},
    is_element(K, add_element(K, T)) == true
  ).

% if k1 /= k2 => is_element k1 (add_element k2 t) = is_element k1 t
prop_add_different_value_does_not_affect_is_element() ->
  ?FORALL( {T, K1, K2},
    {trie_set(string()), string(), string()},
    ?IMPLIES( K1 =/= K2,
      is_element(K1, add_element(K2, T)) == is_element(K1, T)
    )).

% starts_with k1 (add k1 ++ k2 t)
prop_starts_with_substring_added() ->
  ?FORALL( {T, K1, K2},
    {trie_set(string()), string(), string()},
    starts_with(K1, add_element(K1 ++ K2, T)) == true
  ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
trie_set(Gen) -> ?LET(L, list(Gen), from_list(L)).
