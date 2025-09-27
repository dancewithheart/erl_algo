-module(prop_trie).
-include_lib("proper/include/proper.hrl").

-define(NANI, ne).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%

% get k empty = nani
prop_get_empty_gives_default() ->
  ?FORALL( K, string(),
    trie:get(K, trie:new()) == ?NANI
  ).

% get k (put k v t) = v
prop_get_puted_gives_puted_elem() ->
  ?FORALL( {T,V,K}, {trie(integer()), integer(), string()},
    trie:get(K, trie:put(K,V,T)) == V
  ).

% % if k ≠ k' => get k' (put k v t) = get k' t  

prop_put_different_key_not_affect_get() ->
  ?FORALL( {T,V,K,K2},
    {trie(integer()), integer(), string(), string()},
    ?IMPLIES( K =/= K2,
      trie:get(K2, trie:put(K,V,T)) == trie:get(K2,T)
    )).

%%%%%%%%%%%%%%%
%%% Helpers %%%

addStrValues(L) -> lists:map(fun(E) -> {integer_to_list(E), E} end, L).

% %%%%%%%%%%%%%%%%%%
% %%% Generators %%%

trie(Gen) -> ?LET(L, list(Gen), trie:from_list(addStrValues(L))).
