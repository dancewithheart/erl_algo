-module(prop_red_black_tree).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%

prop_after_insert_is_bst() ->
  ?FORALL( {L,K,V}, {red_black_gen(integer()), integer(), string()},
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

% prop_red_black_tree_after_balance_is_bst() ->
%   ?FORALL( {L,R,K,V,C}, {red_black_gen(integer()), red_black_gen(integer()), integer(), string(), color_gen()},
%     red_black_tree:is_bst(red_black_tree:balance(C,L,K,V,R))
%   ).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%

red_black_gen(Gen) ->
  ?LET(L, list(Gen), list_to_red_black_tree(L)).

% color_gen() ->
%   ?LET(B, boolean(), bool_to_color(B)).

%%%%%%%%%%%%%%%
%%% Helpers %%%

% bool_to_color(true) -> red;
% bool_to_color(false) -> black.

list_to_red_black_tree([]) -> red_black_tree:empty();
list_to_red_black_tree(XS) ->
  YS = lists:map(fun(E) -> {E, integer_to_list(E)} end, XS),
  red_black_tree:from_list(YS).

%% TODO balance preserves ForallT : ForallT P l → ForallT P r → P k v → ForallT P (balance c l k v r).
% ins preserves BST : BST t → BST (ins k v t).

% lookup spec
% lookup d k empty_tree = d
% lookup d k (insert k v t) = v
% lookup d k' (insert k v t) = lookup d k' t       if k ≠ k'

% red-black invariants hold:
% Local Invariant: No red node has a red child.
% Global Invariant: Every path from the root to a leaf has the same number of black nodes.
