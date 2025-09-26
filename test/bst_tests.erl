-module(bst_tests).
-include_lib("eunit/include/eunit.hrl").

%% unit tests for Binary Search Tree (BST)

tree_test_() ->
  BST =
    unsafe_tree(
      bst:new(2, "two"),
      4, "four",
      bst:new(5, "five")),
    [test_put(BST),
      test_get_key_in_left_subtree(BST),
      test_get_key_in_right_subtree(BST),
      test_get_missing_key(BST),
      test_is_key_missing_key(BST),
      test_is_bst_holds_for_valid_bst(BST),
      test_is_bst_detect_invalid_bst(),
      test_elements_create_assoc_list(BST)].

test_put(BST) ->
  ?_assertEqual(bst:put(5, "five", (bst:put(2,"two",(bst:new(4,"four"))))), BST).
test_get_key_in_right_subtree(BST) ->
  ?_assertEqual("five", bst:get("", 5, BST)).
test_get_key_in_left_subtree(BST) ->
  ?_assertEqual("two", bst:get("", 2, BST)).
test_get_missing_key(BST) ->
  ?_assertEqual("", bst:get("", 3, BST)).
test_is_key_missing_key(BST) ->
  ?_assertEqual(false, bst:is_key(3, BST)).
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
