%% Binary Search Tree
%% based on formally verified in Rocq in
%% Software Foundations Volume 3: Verified Functional Algorithms
%% by Andrew W. Appel
%% https://softwarefoundations.cis.upenn.edu/vfa-current/SearchTree.html
-module(bst).
-export([
    empty/0, new/2,
    is_key/2, get/3, put/3,
    delete/2,
    all/2, any/2,
    mapVal/2,
    merge/2,
    elements/1,
    inorder_traversal/1, preorder_traversal/1, postorder_traversal/1,
    from_list/1, from_sorted_list/1,
    is_bst/1, equal/2,
    max_depth/1, min_depth/1]).
-export_type([emptyBst/0, bstNode/2, bst/2, predicate/2]).
-record(tree, {left, key, value, right}).

-type emptyBst() :: empty.
-type bstNode(K,V) :: #tree{
    left :: bst(K, V),
    key :: K,
    value :: V,
    right :: bst(K, V)}.
-type bst(K, V) :: emptyBst() | bstNode(K,V).
-type predicate(K,V) :: fun((K,V) -> boolean()).

%% @doc create empty BST
-spec empty() -> emptyBst().
empty() -> empty.

-spec new(bst(K,V), K, V, bst(K,V)) -> bstNode(K,V).
new(L,K,V,R) -> #tree{left = L, key = K, value = V, right = R}.

%% @doc create new BST with K := V
-spec new(K,V) -> bstNode(K,V).
new(Key, Value) -> new(empty, Key, Value, empty).

%% @doc check if K is bound in BST
-spec is_key(K, bst(K,_V)) -> boolean().
is_key(_, empty) -> false;
is_key(K, #tree{left = L, key = K2}) when K < K2 -> is_key(K, L);
is_key(K, #tree{key = K2, right = R}) when K > K2 -> is_key(K, R);
is_key(K, #tree{key = K}) -> true.

%% @doc get value bound to K in tree, or default value D
-spec get(V, K, bst(K,V)) -> V.
get(D, _K, empty) -> D;
get(D, K, #tree{left = L, key = K2}) when K < K2 -> get(D,K,L);
get(D, K, #tree{key = K2, right = R}) when K > K2 -> get(D,K,R);
get(_, K, #tree{key = K, value = V}) -> V.

%% @doc insert key := value into the BST
-spec put({K, V}, bst(K,V)) -> bstNode(K,V).
put({K, V}, T) -> put(K,V,T).

%% @doc insert key := value into the BST
-spec put(K, V, bst(K,V)) -> bstNode(K,V).
put(K, V, empty) -> new(K,V);
put(K, V, T = #tree{left = L, key = K2}) when K < K2
  -> T#tree{left = put(K, V, L)};
put(K, V, T = #tree{key = K2, right = R}) when K > K2
  -> T#tree{right = put(K, V, R)};
put(K, V, T = #tree{key = K})
  -> T#tree{value = V}.

% @TODO change to equivalent regardless of structure
%% @doc check if two BST are equal
-spec equal(bst(K,V), bst(K,V)) -> boolean().
equal(empty, empty) -> true;
equal(#tree{}, empty) -> false;
equal(empty,  #tree{}) -> false;
equal( #tree{left = L, key = K, value = V, right = R},
       #tree{left = L2, key = K, value = V, right = R2})
  -> equal(L,L2) andalso equal(R,R2);
equal(_, _) -> false.

%% @doc check if predicate P(K,V) is true for every pair in BST
-spec all(predicate(K,V), bst(K,V)) -> boolean().
all(_, empty) -> true;
all(P, #tree{left = L, key = K, value = V, right = R}) ->
  P(K,V) andalso all(P, L) andalso all(P, R).

%% @doc check if predicate P(K,V) is true for any pair in BST
-spec any(predicate(K,V), bst(K,V)) -> boolean().
any(_, empty) -> false;
any(P, #tree{left = L, key = K, value = V, right = R}) ->
  P(K,V) orelse any(P, L) orelse any(P, R).

%% @doc check if argument is BST
-spec is_bst(bst(_K,_V)) -> boolean().
is_bst(empty) -> true;
is_bst(#tree{left = L, key = K, right = R}) ->
    all(fun(K2,_) -> K2 < K end, L)
    andalso all(fun(K2,_) -> K2 > K end, R)
    andalso is_bst(L) andalso is_bst(R).

%% @doc converts BST to association list - in order traversals
-spec elements(bst(K,V)) -> [{K,V}].
elements(T) -> inorder_traversal(T, []).

%% @doc converts BST to association list using in-order traversals
-spec inorder_traversal(bst(K,V)) -> [{K,V}].
inorder_traversal(T) -> inorder_traversal(T, []).

inorder_traversal(empty, Acc) -> Acc;
inorder_traversal(#tree{left = L, key = K, value = V, right = R}, Acc) ->
  inorder_traversal(L, [{K,V}|inorder_traversal(R, Acc)]).

%% @doc converts BST to association list using pre-order traversals
-spec preorder_traversal(bst(K,V)) -> [{K,V}].
preorder_traversal(T) -> preorder_traversal(T, []).

preorder_traversal(empty, Acc) -> Acc;
preorder_traversal(#tree{left = L, key = K, value = V, right = R}, Acc) ->
  [{K,V}|preorder_traversal(L, preorder_traversal(R,Acc))].

%% @doc converts BST to association list using post-order traversals
-spec postorder_traversal(bst(K,V)) -> [{K,V}].
postorder_traversal(T) -> postorder_traversal(T, []).

postorder_traversal(empty, Acc) -> Acc;
postorder_traversal(#tree{left = L, key = K, value = V, right = R}, Acc) ->
  postorder_traversal(L, postorder_traversal(R, [{K,V}|Acc])).

%% @doc converts list to BST
-spec from_list([{K,V}]) -> bst(K,V).
from_list(XS) -> lists:foldl(fun put/2, empty, XS).

%% @doc converts sorted list to BST
-spec from_sorted_list([{K,V}]) -> bst(K,V).
from_sorted_list([]) -> empty;
from_sorted_list([{K,V}]) -> new(K, V);
from_sorted_list(L) ->
  Mid = length(L) div 2,
  {L1,[{K,V}|L2]} = lists:split(Mid, L),
  new(
    from_sorted_list(L1),
    K, V,
    from_sorted_list(L2)).

%% @doc combine two BST into single BST
-spec merge(bst(K,V), bst(K,V)) -> bst(K,V).
merge(X, empty) -> X;
merge(empty, X) -> X;
merge(#tree{left = L1, key = K1, value = V1, right = R1}, #tree{left = L2, key = K2, value = V2, right = R2}) when K1 < K2 ->
  N = new(L1, K1, V1, merge(put(K2, V2, R1), R2)),
  merge(N, L2);
merge(#tree{left = L1, key = K, right = R1}, #tree{left = L2, key = K, right = R2} = T)
  -> T#tree{left = merge(L1,L2), right = merge(R1,R2)};
merge(#tree{left = L1, key = K1} = T, #tree{left = L2, key = K2, value = V2, right = R2}) when K1 > K2 ->
  N = T#tree{left = merge(put(K2, V2, L1),L2)},
  merge(N, R2).

% TODO delete from BST -> filter

%% @doc delete entry with key K from BST
-spec delete(T :: bst(K,V), Key :: K) -> bst(K,V).
delete(_, empty) -> empty;
delete(X, #tree{key = X, left = empty, right = R}) -> R;
delete(X, #tree{key = X, left = L, right = empty}) -> L;
delete(X, #tree{key = X, left = L, right = R}) ->
  {{K2, V2}, R2} = delete_min(R),
  #tree{left = L, key = K2, value = V2, right = R2};
delete(X, #tree{key = K, left = L} = T) when X < K ->
  T#tree{left = delete(X, L)};
delete(X, #tree{key = K, right = R} = T) when X > K ->
  T#tree{right = delete(X, R)}.

%% @doc delete entry with smallest key from BST, returns deleted entry and new tree
delete_min(#tree{key = K, value = V, left = empty, right = empty}) -> {{K,V}, empty};
delete_min(#tree{key = K, value = V, left = empty, right = R}) -> {{K,V}, R};
delete_min(T = #tree{left = L}) ->
  {{K2, V2}, L2} = delete_min(L),
  {{K2, V2}, T#tree{left = L2}}.

%% @doc map values of BST
-spec mapVal(fun((A) -> B), bst(K,A)) -> bst(K,B).
mapVal(_, empty) -> empty;
mapVal(F, #tree{left = L, key = K, value = V, right = R}) ->
  new(mapVal(F, L), K, F(V), mapVal(F, R)).

%% @doc max depth of a path from root to leaf or in BST (height of BST)
-spec max_depth(bst(_K,_V)) -> non_neg_integer().
max_depth(empty) -> 0;
max_depth(#tree{left = L, right = R}) ->
  1 + max(max_depth(L), max_depth(R)).

%% @doc min depth of a path from root to leaf in BST
-spec min_depth(bst(_K,_V)) -> non_neg_integer().
min_depth(empty) -> 0;
min_depth(#tree{left = empty, right = empty}) -> 1;
min_depth(#tree{left = empty, right = R}) -> 1 + min_depth(R);
min_depth(#tree{left = L, right = empty}) -> 1 + min_depth(L);
min_depth(#tree{left = L, right = R}) -> 1 + min(min_depth(L), min_depth(R)).

% TODO folds -> based on traversals
