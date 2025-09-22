-module(bst_tests).
-import(bst, [empty/0, new/4, bound/2, insert/3, equal/2]).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

tree_test_() ->
    BST = bst:new(
        bst:new(bst:empty(), 2, "two", bst:empty()),
        4, "four",
        bst:new(bst:empty(), 5, "five", bst:empty())),
    [test_insert(BST),
        test_lookup_key_in_left_subtree(BST),
        test_lookup_key_in_right_subtree(BST),
        test_lookup_missing_key(BST),
        test_bound_missing_key(BST),
        test_isBST_holds_for_valid_BST(BST),
        test_isBST_detect_invalid_BST(),
        test_elements_create_assoc_list(BST)].

test_insert(BST) ->
  ?_assertEqual(bst:insert(5, "five", (bst:insert(2,"two",(bst:insert(4,"four", bst:empty()))))), BST).
test_lookup_key_in_right_subtree(BST) ->
  ?_assertEqual("five", bst:lookup("", 5, BST)).
test_lookup_key_in_left_subtree(BST) ->
  ?_assertEqual("two", bst:lookup("", 2, BST)).
test_lookup_missing_key(BST) ->
  ?_assertEqual("", bst:lookup("", 3, BST)).
test_bound_missing_key(BST) ->
  ?_assertEqual(false, bst:bound(3, BST)).
test_isBST_holds_for_valid_BST(BST) ->
  ?_assertEqual(true, bst:isBST(BST)).
test_isBST_detect_invalid_BST() ->
  ?_assertEqual(false,
    bst:isBST(bst:new(
      bst:new(bst:empty(), 5, "five", bst:empty()),
      4, "four",
      bst:new(bst:empty(), 2, "two", bst:empty())
    ))
).
test_elements_create_assoc_list(BST) ->
  ?_assertEqual([{2, "two"}, {4, "four"}, {5, "five"}], bst:elements(BST)).

validBst(Gen) ->
    ?LET(L, list(Gen), list_to_bst(L)).

list_to_bst([]) -> bst:empty();
list_to_bst(XS) ->
  lists:foldl(fun(E, Acc) -> bst:insert(E, integer_to_list(E), Acc) end, bst:empty(), XS).

% if we insert all elements using bst:insert then we have valid BST
% forall k,v,t. isBST(t) => isBST(insert(k,v,t))
prop_BST_after_insert_is_BST() ->
  ?FORALL(L, validBst(integer()),
  bst:isBST(L)).

% lookup d k empty_tree = d
prop_lookup_empty_BST_gives_default() ->
  ?FORALL({D,K}, {string(), integer()},
  bst:lookup(D, K, bst:empty()) == D ).

% lookup d k (insert k v t) = v
prop_lookup_inserted_gives_inserted_elem() ->
  ?FORALL({L,D,V,K}, {validBst(integer()),string(),string(), integer()},
  bst:lookup(D, K, (bst:insert(K, V, L))) == V ).

% if k ≠ k' => lookup d k' (insert k v t) = lookup d k' t  

prop_insert_different_key_not_affect_lookup() ->
  ?FORALL(
    {L,D,V,K,K2},
    {validBst(integer()),string(),string(),integer(),integer()},
    ?IMPLIES(
      K =/= K2,
      bst:lookup(D, K2, (bst:insert(K, V, L))) == bst:lookup(D, K2, L) )).

% lookup d k' (insert k v (insert k v' t)) = lookup d k' (insert k v t)
% lookup d k' (insert k (lookup d k t) t) = lookup d k' t.
%  k1 ≠ k2 =>
%   lookup d k' (insert k1 v1 (insert k2 v2 t))
%    =
%   lookup d k' (insert k2 v2 (insert k1 v1 t))
