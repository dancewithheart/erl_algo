-module(bst_tests).
-include_lib("eunit/include/eunit.hrl").

%% unit tests for Binary Search Tree (BST)

tree_test_() ->
  BST =
    unsafe_tree(
      bst:new(2, "two"),
      4, "four",
      bst:new(5, "five")),
  %             5
  %     2             7
  %  1     3        6   8
  %          4             9
  BSTBig = unsafe_tree(
      unsafe_tree(bst:new(1, "1"), 2, "2", unsafe_tree(empty, 3, "3", bst:new(4, "4"))),
    5, "5",
      unsafe_tree(bst:new(6, "6"), 7, "7", unsafe_tree(empty, 8, "8", bst:new(9, "9")))),
    [test_put(BST),
      test_get_key_in_left_subtree(BST),
      test_get_key_in_right_subtree(BST),
      test_get_missing_key(BST),
      test_is_key_missing_key(BST),
      test_is_bst_holds_for_valid_bst(BST),
      test_is_bst_detect_invalid_bst(),
      test_elements_create_assoc_list(BSTBig),
      test_inorder_traversal(BSTBig),
      test_preorder_traversal(BSTBig),
      test_preorder_traversal2(BSTBig),
      test_postorder_traversal(BSTBig)].

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
test_elements_create_assoc_list(BST) -> ?_assertEqual(
  [{1,"1"}, {2,"2"}, {3,"3"}, {4,"4"}, {5,"5"}, {6,"6"}, {7,"7"}, {8,"8"}, {9,"9"}],
  bst:elements(BST)
).

test_inorder_traversal(BST) -> ?_assertEqual(
  [{1,"1"}, {2,"2"}, {3,"3"}, {4,"4"}, {5,"5"}, {6,"6"}, {7,"7"}, {8,"8"}, {9,"9"}],
  bst:inorder_traversal(BST)
).

test_preorder_traversal(BST) -> ?_assertEqual(
  [{5,"5"}, {2,"2"}, {1,"1"}, {3,"3"}, {4,"4"}, {7,"7"}, {6,"6"}, {8,"8"}, {9,"9"}],
  bst:preorder_traversal(BST)
).

test_preorder_traversal2(BST) -> ?_assertEqual(
  [{5,"5"}, {2,"2"}, {1,"1"}, {3,"3"}, {4,"4"}, {7,"7"}, {6,"6"}, {8,"8"}, {9,"9"}],
  bst:preorder_traversal(BST)
).

test_postorder_traversal(BST) -> ?_assertEqual(
  [{1,"1"}, {4,"4"}, {3,"3"}, {2,"2"}, {6,"6"}, {9,"9"}, {8,"8"}, {7,"7"}, {5,"5"}],
  bst:postorder_traversal(BST)
).

unsafe_tree(L,K,V,R) -> {tree, L, K, V, R}.
