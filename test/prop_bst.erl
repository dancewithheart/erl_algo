-module(prop_bst).
-include_lib("proper/include/proper.hrl").

%% property tests for Binary Search Tree (BST)

%%%%%%%%%%%%%%%%%%
%%% Properties %%%

% is_bst(t) => is_bst(insert(k,v,t))
prop_bst_after_insert_is_bst() ->
  ?FORALL( {L,K,V}, {bst_gen(integer()), integer(), string()},
    bst:is_bst(bst:insert(K,V,L))
  ).

% lookup d k empty_tree = d
prop_lookup_empty_bst_gives_default() ->
  ?FORALL( {D,K}, {string(), integer()},
    bst:lookup(D, K, bst:empty()) == D
  ).

% lookup d k (insert k v t) = v
prop_lookup_inserted_gives_inserted_elem() ->
  ?FORALL( {L,D,V,K}, {bst_gen(integer()),string(),string(), integer()},
    bst:lookup(D, K, (bst:insert(K,V,L))) == V
  ).

% if k ≠ k' => lookup d k' (insert k v t) = lookup d k' t  

prop_insert_different_key_not_affect_lookup() ->
  ?FORALL( {L,D,V,K,K2},
    {bst_gen(integer()), string(), string(), integer(), integer()},
    ?IMPLIES( K =/= K2,
      bst:lookup(D, K2, (bst:insert(K,V,L))) == bst:lookup(D,K2,L)
    )).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%

bst_gen(Gen) ->
    ?LET(L, list(Gen), list_to_bst(L)).

%%%%%%%%%%%%%%%
%%% Helpers %%%

list_to_bst([]) -> bst:empty();
list_to_bst(XS) ->
  YS = lists:map(fun(E) -> {E, integer_to_list(E)} end, XS),
  bst:from_list(YS).

% TODO properties for bound

% TODO
% lookup d k' (insert k v (insert k v' t)) = lookup d k' (insert k v t)
% lookup d k' (insert k (lookup d k t) t) = lookup d k' t.
%  k1 ≠ k2 =>
%   lookup d k' (insert k1 v1 (insert k2 v2 t))
%    =
%   lookup d k' (insert k2 v2 (insert k1 v1 t))
