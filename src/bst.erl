%% Binary Search Tree
%% based on formally verified in Rocq in
%% Software Foundations Volume 3: Verified Functional Algorithms
%% by Andrew W. Appel
%% https://softwarefoundations.cis.upenn.edu/vfa-current/SearchTree.html
-module(bst).
-export([
    empty/0, new/2, new/4,
    bound/2, lookup/3, insert/3,
    forall/2,
    elements/1, from_list/1, 
    is_bst/1, equal/2]).
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

-spec empty() -> emptyBst().
empty() -> empty.

-spec new(bst(K,V), K, V, bst(K,V)) -> bstNode(K,V).
new(L,K,V,R) -> #tree{left = L, key = K, value = V, right = R}.

-spec new(K,V) -> bstNode(K,V).
new(Key, Value) -> new(empty(), Key, Value, empty()).

% check if K is bound in BST
-spec bound(K, bst(K,_V)) -> boolean().
bound(_, empty) -> false;
bound(K, #tree{left = L, key = K2}) when K < K2 -> bound(K, L);
bound(K, #tree{key = K2, right = R}) when K > K2 -> bound(K, R);
bound(_,_) -> true.

% value bound to K in tree, or default value D
-spec lookup(V, K, bst(K,V)) -> V.
lookup(D, _K, empty) -> D;
lookup(D, K, #tree{left = L, key = K2}) when K < K2 -> lookup(D,K,L);
lookup(D, K, #tree{key = K2, right = R}) when K > K2 -> lookup(D,K,R);
lookup(_, _, #tree{value = V}) -> V.

-spec insert(V, K, bst(K,V)) -> bstNode(K,V).
insert(K, V, empty) -> new(K,V);
insert(K, V, #tree{left = L, key = K2, value = V2, right = R}) when K < K2
  -> new(insert(K, V, L), K2, V2, R);
insert(K, V, #tree{left = L, key = K2, value = V2, right = R}) when K > K2
  -> new(L, K2, V2, insert(K, V, R));
insert(K, V, #tree{left = L, key = _K2, value = _V2, right = R})
  -> new(L, K, V, R).

-spec equal(bst(K,V), bst(K,V)) -> boolean().
equal(empty, empty) -> true;
equal(#tree{}, empty) -> false;
equal(empty,  #tree{}) -> false;
equal( #tree{left = L, key = K, value = V, right = R},
       #tree{left = L2, key = K2, value = V2, right = R2}) when K == K2, V == V2
  -> equal(L,L2) andalso equal(R,R2);
equal(_, _) -> false.

-spec forall(predicate(K,V), bst(K,V)) -> boolean().
forall(_, empty) -> true;
forall(P, #tree{left = L, key = K, value = V, right = R}) ->
  P(K,V) andalso forall(P, L) andalso forall(P, R).

% check if #tree is proper BST
-spec is_bst(bst(_K,_V)) -> boolean().
is_bst(empty) -> true;
is_bst(#tree{left = L, key = K, right = R}) ->
    forall(fun(K2,_) -> K2 < K end, L)
    andalso forall(fun(K2,_) -> K2 > K end, R)
    andalso is_bst(L) andalso is_bst(R).

% converts BST to association list - in order travrsal
-spec elements(bst(K,V)) -> [{K,V}].
elements(T) -> elements(T, []).

elements(empty, Acc) -> Acc;
elements(#tree{left = L, key = K, value = V, right = R}, Acc) ->
    elements(L, [{K,V} | elements(R, Acc)]).

% converts list to BST
-spec from_list([{K,V}]) -> bst(K,V).
from_list([]) -> bst:empty();
from_list(XS) ->
  lists:foldl(fun({K, V}, Acc) -> bst:insert(K, V, Acc) end, bst:empty(), XS).

% TODO merge two BST
% TODO concat two BST

% TODO delete from BST -> filter

% TODO other traversals e.g. in order
% TODO folds -> based on traversals

% TODO map over values
