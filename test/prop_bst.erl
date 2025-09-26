-module(prop_bst).
-include_lib("proper/include/proper.hrl").

%% property tests for Binary Search Tree (BST)

%%%%%%%%%%%%%%%%%%
%%% Properties %%%

% properties for lookup and insert

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
    bst:lookup(D, K, bst:insert(K,V,L)) == V
  ).

% if k ≠ k' => lookup d k' (insert k v t) = lookup d k' t  

prop_insert_different_key_not_affect_lookup() ->
  ?FORALL( {L,D,V,K,K2},
    {bst_gen(integer()), string(), string(), integer(), integer()},
    ?IMPLIES( K =/= K2,
      bst:lookup(D, K2, bst:insert(K,V,L)) == bst:lookup(D,K2,L)
    )).

% properties for bound and insert

% bound k empty_tree = false
prop_bound_empty_bst_gives_default() ->
  ?FORALL( K, integer(),
    bst:bound(K, bst:empty()) == false
  ).

% bound k (insert k v t) = true
prop_bound_inserted_gives_inserted_elem() ->
  ?FORALL( {L,V,K}, {bst_gen(integer()),string(), integer()},
    bst:bound(K, bst:insert(K,V,L)) == true
  ).

% if k ≠ k' => bound k' (insert k v t) = bound k' t  

prop_insert_different_key_not_affect_bound() ->
  ?FORALL( {L,V,K,K2},
    {bst_gen(integer()), string(), integer(), integer()},
    ?IMPLIES( K =/= K2,
      bst:bound(K2, bst:insert(K,V,L)) == bst:bound(K2,L)
    )).

% properties for bound and lookup

% bound k t == false => lookup d k t == d
prop_if_not_bound_then_lookup_default() ->
  ?FORALL( {T,D,K}, {bst_gen(integer()), string(), integer()},
    case bst:bound(K, T) of
      false -> bst:lookup(D, K, T) == D;
      true -> true
    end
  ).

% (lookup d k t /= d) => bound k t
prop_lookup_not_default_then_bound() ->
  ?FORALL( {T,D,K}, {bst_gen(integer()), string(), integer()},
    case bst:lookup(D, K, T) /= D of
      true -> bst:bound(K, T);
      false -> true
    end
  ).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%

bst_gen(Gen) ->
    ?LET(L, list(Gen), list_to_bst(L)).

%%%%%%%%%%%%%%%
%%% Helpers %%%

list_to_bst([]) -> bst:empty();
list_to_bst(L) -> bst:from_list(addStrValues(L)).

addStrValues(L) -> lists:map(fun(E) -> {E, integer_to_list(E)} end, L).

