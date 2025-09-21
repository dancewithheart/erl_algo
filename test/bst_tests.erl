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
        test_isBST_detect_invalid_BST()].

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

validBst(T) ->
    ?LET(L, list(T), list_to_bst(L)).

list_to_bst([]) -> bst:empty();
list_to_bst(XS) ->
  lists:foldl(fun(E, Acc) -> bst:insert(E, integer_to_list(E), Acc) end, bst:empty(), XS).

% if we insert all elements using bst:insert then we have valid BST
% forall k,v,t. isBST(t) => isBST(insert(k,v,t))
prop_BST_after_insert_is_BST() ->
  ?FORALL(L, validBst(integer()), bst:isBST(L)).

% TODO lookup d k empty_tree = d
% TODO lookup d k (insert k v t) = v
% if k ≠ k' => lookup d k' (insert k v t) = lookup d k' t  
