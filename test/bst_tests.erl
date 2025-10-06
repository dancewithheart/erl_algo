-module(bst_tests).
-include_lib("eunit/include/eunit.hrl").

%% unit tests for Binary Search Tree (BST)

tree_test_() ->
  BST =
    new(
      new(2),
      4,
      new(5)),
  %             5
  %     2             7
  %  1     3        6   8
  %          4             9
  BSTBig = new(
      new(new(1), 2, new(empty, 3, new(4))),
      5,
      new(new(6), 7, new(empty, 8, new(9)))),
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
      test_postorder_traversal(BSTBig),
      test_delete_left(),
      test_delete_right(),
      test_delete_parent_empty_children(),
      test_delete_nested(),
      test_delete_not_existing_key(),
      test_from_sorted_list(),
      test_max_depth(),
      test_min_depth(),
      test_min_depth2()].

test_put(BST) ->
  ?_assertEqual(bst:put(5, "5", (bst:put(2,"2",(bst:new(4,"4"))))), BST).
test_get_key_in_right_subtree(BST) ->
  ?_assertEqual("5", bst:get("", 5, BST)).
test_get_key_in_left_subtree(BST) ->
  ?_assertEqual("2", bst:get("", 2, BST)).
test_get_missing_key(BST) ->
  ?_assertEqual("", bst:get("", 3, BST)).
test_is_key_missing_key(BST) ->
  ?_assertEqual(false, bst:is_key(3, BST)).
test_is_bst_holds_for_valid_bst(BST) ->
  ?_assertEqual(true, bst:is_bst(BST)).
test_is_bst_detect_invalid_bst() ->
  ?_assertEqual(false,
    bst:is_bst(new(
      new(5),
      4,
      new(2)
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

test_delete_left() -> ?_assertEqual(
                         new(4),
  bst:delete(5, new(new(4), 5, bst:empty()))
).

test_delete_right() -> ?_assertEqual(
                                         new(6),
  bst:delete(5, new(bst:empty(), 5, new(6)))
).

test_delete_parent_empty_children() -> ?_assertEqual(
  new(new(4), 6, bst:empty()),
  bst:delete(5, new(new(4), 5, new(6)))
).

test_delete_nested() ->
  ?_assertEqual(
              new(new(new(2), 4, bst:empty()), 5, new(bst:empty(), 6, new(7))),
  bst:delete(3, new(new(new(2), 3, new(4)), 5, new(bst:empty(), 6,  new(7))))
).

% Keep this failing test, I want custom assert - nice visualisations, output in the format I use to insert into tests - use new/2 new/4 etc
% test_delete_node_with_non_emty_children() ->
%   ?_assertEqual(
%   new(new(2), 3, new(bst:empty(), 6, new(7))),
%   bst:delete(new(new(new(2), 3, new(4)), 5, new(bst:empty(), 6,  new(7))), 5)
% ).

test_delete_not_existing_key() ->
  T = new(new(new(2), 3, new(4)), 5, new(bst:empty(), 6,  new(7))),
  ?_assertEqual(T, bst:delete(1, T)
).

test_from_sorted_list() ->
  ?_assertEqual(
    new(new(new(0), 1, new(2)), 3, new(new(4), 5, empty)),
    bst:from_sorted_list(lists:map(fun(E) -> {E, integer_to_list(E)} end, [0,1,2,3,4,5]))
  ).

test_max_depth() ->
  ?_assertEqual(3,
    bst:max_depth(new(new(9), 3, new(new(15), 20, new(7))))
).

test_min_depth() ->
  ?_assertEqual(2,
    bst:min_depth(new(new(9), 3, new(new(15), 20, new(7))))
).

test_min_depth2() ->
  ?_assertEqual(3,
    bst:min_depth(new(empty, 2, new(empty, 3, new(4))))
).

test_value(K) -> integer_to_list(K).
new(K) -> bst:new(K, test_value(K)).
new(L,K,R) -> new(L,K,test_value(K), R).
new(L,K,V,R) -> {tree, L, K, V, R}.
