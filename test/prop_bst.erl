-module(prop_bst).
-include_lib("proper/include/proper.hrl").

%% property tests for Binary Search Tree (BST)

%%%%%%%%%%%%%%%%%%
%%% Properties %%%

% properties for get and put

% is_bst(t) => is_bst(put(k,v,t))
prop_bst_after_put_is_bst() ->
  ?FORALL( {L,K,V}, {bst_gen(integer()), integer(), string()},
    bst:is_bst(bst:put(K,V,L))
  ).

% get d k empty_tree = d
prop_get_empty_bst_gives_default() ->
  ?FORALL( {D,K}, {string(), integer()},
    bst:get(D, K, bst:empty()) == D
  ).

% get d k (put k v t) = v
prop_get_puted_gives_puted_elem() ->
  ?FORALL( {L,D,V,K}, {bst_gen(integer()),string(),string(), integer()},
    bst:get(D, K, bst:put(K,V,L)) == V
  ).

% if k ≠ k' => get d k' (put k v t) = get d k' t  

prop_put_different_key_not_affect_get() ->
  ?FORALL( {L,D,V,K,K2},
    {bst_gen(integer()), string(), string(), integer(), integer()},
    ?IMPLIES( K =/= K2,
      bst:get(D, K2, bst:put(K,V,L)) == bst:get(D,K2,L)
    )).

% properties for is_key and put

% is_key k empty_tree = false
prop_is_key_empty_bst_gives_default() ->
  ?FORALL( K, integer(),
    bst:is_key(K, bst:empty()) == false
  ).

% is_key k (put k v t) = true
prop_is_key_puted_gives_puted_elem() ->
  ?FORALL( {L,V,K}, {bst_gen(integer()),string(), integer()},
    bst:is_key(K, bst:put(K,V,L)) == true
  ).

% if k ≠ k' => is_key k' (put k v t) = is_key k' t  

prop_put_different_key_not_affect_is_key() ->
  ?FORALL( {L,V,K,K2},
    {bst_gen(integer()), string(), integer(), integer()},
    ?IMPLIES( K =/= K2,
      bst:is_key(K2, bst:put(K,V,L)) == bst:is_key(K2,L)
    )).

% properties for is_key and get

% is_key k t == false => get d k t == d
prop_if_not_is_key_then_get_default() ->
  ?FORALL( {T,D,K}, {bst_gen(integer()), string(), integer()},
    case bst:is_key(K, T) of
      false -> bst:get(D, K, T) == D;
      true -> true
    end
  ).

% (get d k t /= d) => is_key k t
prop_get_not_default_then_is_key() ->
  ?FORALL( {T,D,K}, {bst_gen(integer()), string(), integer()},
    case bst:get(D, K, T) /= D of
      true -> bst:is_key(K, T);
      false -> true
    end
  ).

% L has no duplicates => elements(from_list(L)) = sort(L)
prop_sorted_no_dups_list_is_elements_compose_from_list() ->
  ?FORALL(L, list_no_duplicates(integer()),
    bst:elements(bst:from_list(addStrValues(L))) == addStrValues(lists:sort(L))
  ).

% is_key (delete k (put k v t)) = false
prop_is_key_delete_false() ->
  ?FORALL( {T,K,V}, {bst_gen(integer()), integer(), string()},
    begin
      T2 = bst:put(K,V,T),
      bst:is_key(K, bst:delete(K, T2)) == false
    end
  ).

prop_delete_different_key_not_affect_is_key() ->
  ?FORALL( {T,K,K2}, {bst_gen(integer()), integer(), integer()},
    ?IMPLIES( K =/= K2,
      bst:is_key(K, bst:delete(K2, T)) == bst:is_key(K, T)
    )).

% map(id, T) == T
prop_map_id_is_noop() ->
  ?FORALL(T, bst_gen(integer()),
    bst:mapVal(fun id/1, T) == T
  ).

id(A) -> A.

% from_list(map(F, L) == mapVal(F, from_list(F))
prop_map_f_from_list_is_from_list_map() ->
  ?FORALL(L, list(integer()),
    begin
      L2 = addStrValues(L),
      bst:from_list(lists:map(fun foo2/1, L2)) == bst:mapVal(fun foo/1, bst:from_list(L2))
    end
  ).

foo2({A,B}) -> {A, foo(B)}.
foo(B) -> string:reverse(B) ++ "foo".

% from_list(X ++ Y) == merge(from_list(X), from_list(Y))
prop_merge_like_list_concat() ->
  ?FORALL({X,Y}, {list(integer()), list(integer())},
    begin
      Y2 = addStrValues(Y),
      X2 = addStrValues(X),
      bst:from_list(X2 ++ Y2) == bst:merge(bst:from_list(X2), bst:from_list(Y2))
    end
  ).

prop_sorted_postorder_traversal_and_preorder_traversal_same() ->
  ?FORALL(
    T1,
    bst_gen(integer()),
    begin
      lists:sort(lists:map(fun({N,_}) -> N end, bst:postorder_traversal(T1))) ==
      lists:sort(lists:map(fun({N,_}) -> N end, bst:preorder_traversal(T1)))
    end).

prop_sorted_postorder_traversal_and_inorder_traversal_same() ->
  ?FORALL(
    T1,
    bst_gen(integer()),
    begin
      lists:sort(lists:map(fun({N,_}) -> N end, bst:postorder_traversal(T1))) ==
      lists:sort(lists:map(fun({N,_}) -> N end, bst:inorder_traversal(T1)))
    end).

prop_height_from_sorted_list_is_O_log2() ->
  ?FORALL( L, list(integer()),
    begin
      L2 = addStrValues(lists:usort(L)),
      bst:max_depth(bst:from_sorted_list(L2)) =< math:log2(2 * length(L2) + 1)
    end).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%

bst_gen(Gen) -> ?LET(L, list(Gen), list_to_bst(L)).

list_no_duplicates(T) -> ?LET(L, list(T), remove_duplicates(L)).

%%%%%%%%%%%%%%%
%%% Helpers %%%

list_to_bst(L) -> bst:from_list(addStrValues(L)).

addStrValues(L) -> lists:map(fun(E) -> {E, integer_to_list(E)} end, L).

remove_duplicates([]) -> [];
remove_duplicates([H|T]) ->
  case lists:member(H,T) of
    true -> remove_duplicates(T);
    false -> [H|remove_duplicates(T)]
  end.
