-module(prop_red_black_tree).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%

prop_after_put_is_red_black() ->
  ?FORALL( {L,V,K}, {red_black_gen(integer()), string(), integer()},
    red_black_tree:is_red_black(red_black_tree:put(K,V,L))
  ).

prop_after_put_is_bst() ->
  ?FORALL( {L,V,K}, {red_black_gen(integer()), string(), integer()},
    red_black_tree:is_bst(red_black_tree:put(K,V,L))
  ).

% get d k empty_tree = d
prop_get_empty_gives_default() ->
  ?FORALL( {D,K}, {string(), integer()},
    red_black_tree:get(D, K, red_black_tree:empty()) == D
  ).

% get d k (put k v t) = v
prop_get_put_returns_elem() ->
  ?FORALL( {L,D,V,K}, {red_black_gen(integer()),string(),string(), integer()},
    red_black_tree:get(D, K, (red_black_tree:put(K,V,L))) == V
  ).

% if k ≠ k' => get d k' (put k v t) = get d k' t  

prop_put_different_key_not_affect_get() ->
  ?FORALL( {L,D,V,K,K2},
    {red_black_gen(integer()), string(), string(), integer(), integer()},
    ?IMPLIES( K =/= K2,
      red_black_tree:get(D, K2, (red_black_tree:put(K,V,L))) == red_black_tree:get(D,K2,L)
    )).

% properties for is_key and put

% is_key k empty_tree = false
prop_is_key_empty_bst_gives_default() ->
  ?FORALL( K, integer(),
    red_black_tree:is_key(K, red_black_tree:empty()) == false
  ).

% is_key k (put k v t) = true
prop_is_key_puted_gives_puted_elem() ->
  ?FORALL( {L,V,K}, {red_black_gen(integer()),string(), integer()},
    red_black_tree:is_key(K, red_black_tree:put(K,V,L)) == true
  ).

% if k ≠ k' => is_key k' (put k v t) = is_key k' t  

prop_put_different_key_not_affect_is_key() ->
  ?FORALL( {L,V,K,K2},
    {red_black_gen(integer()), string(), integer(), integer()},
    ?IMPLIES( K =/= K2,
      red_black_tree:is_key(K2, red_black_tree:put(K,V,L)) == red_black_tree:is_key(K2,L)
    )).

% properties for is_key and get

% is_key k t == false => get d k t == d
prop_if_not_is_key_then_get_default() ->
  ?FORALL( {T,D,K}, {red_black_gen(integer()), string(), integer()},
    case red_black_tree:is_key(K, T) of
      false -> red_black_tree:get(D, K, T) == D;
      true -> true
    end
  ).

% (get d k t /= d) => is_key k t
prop_get_not_default_then_is_key() ->
  ?FORALL( {T,D,K}, {red_black_gen(integer()), string(), integer()},
    case red_black_tree:get(D, K, T) /= D of
      true -> red_black_tree:is_key(K, T);
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
