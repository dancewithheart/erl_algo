-module(bst_tests).
-include_lib("eunit/include/eunit.hrl").

%% unit tests for Binary Search Tree (BST)

tree_test_() ->
  BST =
    unsafe_tree(
      bst:new(2, "two"),
      4, "four",
      bst:new(5, "five")),
    [test_insert(BST),
      test_lookup_key_in_left_subtree(BST),
      test_lookup_key_in_right_subtree(BST),
      test_lookup_missing_key(BST),
      test_bound_missing_key(BST),
      test_is_bst_holds_for_valid_bst(BST),
      test_is_bst_detect_invalid_bst(),
      test_elements_create_assoc_list(BST)].

test_insert(BST) ->
  ?_assertEqual(bst:insert(5, "five", (bst:insert(2,"two",(bst:new(4,"four"))))), BST).
test_lookup_key_in_right_subtree(BST) ->
  ?_assertEqual("five", bst:lookup("", 5, BST)).
test_lookup_key_in_left_subtree(BST) ->
  ?_assertEqual("two", bst:lookup("", 2, BST)).
test_lookup_missing_key(BST) ->
  ?_assertEqual("", bst:lookup("", 3, BST)).
test_bound_missing_key(BST) ->
  ?_assertEqual(false, bst:bound(3, BST)).
test_is_bst_holds_for_valid_bst(BST) ->
  ?_assertEqual(true, bst:is_bst(BST)).
test_is_bst_detect_invalid_bst() ->
  ?_assertEqual(false,
    bst:is_bst(unsafe_tree(
      bst:new(5, "five"),
      4, "four",
      bst:new(2, "two")
    ))
).
test_elements_create_assoc_list(BST) ->
  ?_assertEqual([{2, "two"}, {4, "four"}, {5, "five"}], bst:elements(BST)).

unsafe_tree(L,K,V,R) -> {tree, L, K, V, R}.
