%% Binary Search Tree
%% based on formally verified in Rocq in
%% Software Foundations Volume 3: Verified Functional Algorithms
%% by Andrew W. Appel
%% https://softwarefoundations.cis.upenn.edu/vfa-current/SearchTree.html
-module(bst).
-export([empty/0, new/2, new/4, bound/2, lookup/3, insert/3, equal/2,
    forall/2, is_bst/1, elements/1]).
-record(tree, {left, key, value, rigt}).
% #tree{left = L, key = K, value = V, rigt = R}
% tree ::= bstEmpty | l: tree, k: K, v: V, r: tree

empty() -> empty.

new(L,K,V,R) -> #tree{left = L, key = K, value = V, rigt = R}.

% new(T,V) -> bst(T)
new(Key, Value) -> new(empty(), Key, Value, empty()).

% check if K is bound in BST
bound(_, empty) -> false;
bound(K, #tree{left = L, key = K2}) when K < K2 -> bound(K, L);
bound(K, #tree{key = K2, rigt = R}) when K > K2 -> bound(K, R);
bound(_,_) -> true.

% lookup(V, K, tree): bool()
% value bound to K in tree, or default value D
lookup(D, _K, empty) -> D;
lookup(D, K, #tree{left = L, key = K2}) when K < K2 -> lookup(D,K,L);
lookup(D, K, #tree{key = K2, rigt = R}) when K > K2 -> lookup(D,K,R);
lookup(_, _, #tree{value = V}) -> V.

insert(K, V, empty) -> new(K,V);
insert(K, V, #tree{left = L, key = K2, value = V2, rigt = R}) when K < K2
  -> new(insert(K, V, L), K2, V2, R);
insert(K, V, #tree{left = L, key = K2, value = V2, rigt = R}) when K > K2
  -> new(L, K2, V2, insert(K, V, R));
insert(K, V, #tree{left = L, key = _K2, value = _V2, rigt = R})
  -> new(L, K, V, R).

equal(empty, empty) -> true;
equal(#tree{}, empty) -> false;
equal(empty,  #tree{}) -> false;
equal( #tree{left = L, key = K, value = V, rigt = R},
       #tree{left = L2, key = K2, value = V2, rigt = R2}) when K == K2, V == V2
  -> equal(L,L2) andalso equal(R,R2);
equal(_, _) -> false.

% forall(P: fun(K,V) -> bool(), t: tree()) -> bool()
forall(_, empty) -> true;
forall(P, #tree{left = L, key = K, value = V, rigt = R}) ->
  P(K,V) andalso forall(P, L) andalso forall(P, R).

% check if #tree is proper BST
is_bst(empty) -> true;
is_bst(#tree{left = L, key = K, rigt = R}) ->
    forall(fun(K2,_) -> K2 < K end, L)
    andalso forall(fun(K2,_) -> K2 > K end, R)
    andalso is_bst(L) andalso is_bst(R).

% converts BST to association list - in order travrsal
elements(T) -> elements(T, []).

elements(empty, Acc) -> Acc;
elements(#tree{left = L, key = K, value = V, rigt = R}, Acc) ->
    elements(L, [{K,V} | elements(R, Acc)]).
