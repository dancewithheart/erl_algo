-module(prop_red_black_tree).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%

prop_after_insert_is_red_black() ->
  ?FORALL( {L,V,K}, {red_black_gen(integer()), string(), integer()},
    red_black_tree:is_red_black(red_black_tree:insert(K,V,L))
  ).

prop_after_insert_is_bst() ->
  ?FORALL( {L,V,K}, {red_black_gen(integer()), string(), integer()},
    red_black_tree:is_bst(red_black_tree:insert(K,V,L))
  ).

% lookup d k empty_tree = d
prop_lookup_empty_gives_default() ->
  ?FORALL( {D,K}, {string(), integer()},
    red_black_tree:lookup(D, K, red_black_tree:empty()) == D
  ).

% lookup d k (insert k v t) = v
prop_lookup_inserted_gives_inserted_elem() ->
  ?FORALL( {L,D,V,K}, {red_black_gen(integer()),string(),string(), integer()},
    red_black_tree:lookup(D, K, (red_black_tree:insert(K,V,L))) == V
  ).

% if k ≠ k' => lookup d k' (insert k v t) = lookup d k' t  

prop_insert_different_key_not_affect_lookup() ->
  ?FORALL( {L,D,V,K,K2},
    {red_black_gen(integer()), string(), string(), integer(), integer()},
    ?IMPLIES( K =/= K2,
      red_black_tree:lookup(D, K2, (red_black_tree:insert(K,V,L))) == red_black_tree:lookup(D,K2,L)
    )).

% properties for bound and insert

% bound k empty_tree = false
prop_bound_empty_bst_gives_default() ->
  ?FORALL( K, integer(),
    red_black_tree:bound(K, red_black_tree:empty()) == false
  ).

% bound k (insert k v t) = true
prop_bound_inserted_gives_inserted_elem() ->
  ?FORALL( {L,V,K}, {red_black_gen(integer()),string(), integer()},
    red_black_tree:bound(K, red_black_tree:insert(K,V,L)) == true
  ).

% if k ≠ k' => bound k' (insert k v t) = bound k' t  

prop_insert_different_key_not_affect_bound() ->
  ?FORALL( {L,V,K,K2},
    {red_black_gen(integer()), string(), integer(), integer()},
    ?IMPLIES( K =/= K2,
      red_black_tree:bound(K2, red_black_tree:insert(K,V,L)) == red_black_tree:bound(K2,L)
    )).

% properties for bound and lookup

% bound k t == false => lookup d k t == d
prop_if_not_bound_then_lookup_default() ->
  ?FORALL( {T,D,K}, {red_black_gen(integer()), string(), integer()},
    case red_black_tree:bound(K, T) of
      false -> red_black_tree:lookup(D, K, T) == D;
      true -> true
    end
  ).

% (lookup d k t /= d) => bound k t
prop_lookup_not_default_then_bound() ->
  ?FORALL( {T,D,K}, {red_black_gen(integer()), string(), integer()},
    case red_black_tree:lookup(D, K, T) /= D of
      true -> red_black_tree:bound(K, T);
      false -> true
    end
  ).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%

red_black_gen(Gen) ->
  ?LET(L, list(Gen), list_to_red_black_tree(L)).

%%%%%%%%%%%%%%%
%%% Helpers %%%

list_to_red_black_tree([]) -> red_black_tree:empty();
list_to_red_black_tree(XS) ->
  YS = lists:map(fun(E) -> {E, integer_to_list(E)} end, XS),
  red_black_tree:from_list(YS).
