%% @doc Red Black Tree implemented based on
%% Software Foundations, Volume 3, Verified Functional Algorithms
%% Andrew W. Appel
%% https://softwarefoundations.cis.upenn.edu/vfa-current/Redblack.html
%% Red Black Tree is self-balancing binary search tree, each node has color
-module(red_black_tree).
-export([lookup/3, insert/2, insert/3,
    is_empty/1, is_leaf/1, bound/2,
    forall/2,
    is_red_black/1, is_bst/1,
    from_list/1, elements/1]).
-export([empty/0]).
-record(rbTree, {color, left, key, value, right}).
% #rbTree{color=C, left=L, key=K, value=V, right=R}

-type color() :: red | black.
-type emptyRBTree() :: empty.
-type redBlackNode(K,V) :: #rbTree{
   color :: color(),
   left :: redblacktree(K, V),
   key :: K,
   value :: V,
   right :: redblacktree(K, V)}.
-type redblacktree(K, V) :: emptyRBTree() | redBlackNode(K,V).

%% @doc create empty Red Black Tree
-spec empty() -> emptyRBTree().
empty() -> empty.

%% @doc true if Red Black Tree is empty
is_empty(empty) -> true;
is_empty(_) -> false.

%% @doc true if Red Black Tree is a leaf
is_leaf(#rbTree{left=empty, right=empty}) -> true;
is_leaf(_) -> false.

node(C,L,K,V,R) -> #rbTree{color=C, left=L, key=K, value=V, right=R}.
red(L,K,V,R) -> node(red,L,K,V,R).
black(L,K,V,R) -> node(black,L,K,V,R).

%% @doc check if given key K exists in Red Black Tree
-spec bound(K, redblacktree(K,_V)) -> boolean().
bound(_, empty) -> false;
bound(X, #rbTree{left=L, key=K}) when X < K -> bound(X,L);
bound(X, #rbTree{key=K, right=R}) when X > K -> bound(X,R);
bound(_, _) -> true.

%% @doc get value bound to K in Red Black Tree, or default value D
-spec lookup(V, K, redblacktree(K,V)) -> V.
lookup(D, _X, empty) -> D;
lookup(D, X, #rbTree{color=_C, left=L, key=K, value=_V, right=_R}) when X < K -> lookup(D,X,L);
lookup(D, X, #rbTree{color=_C, left=_L, key=K, value=_V, right=R}) when X > K -> lookup(D,X,R);
lookup(_D, _X, #rbTree{color=_C, left=_L, key=_K, value=V, right=_R}) -> V.

%% @doc insert K := V into the Red Black Tree
-spec insert({K, V}, redblacktree(K,V)) -> redBlackNode(K,V).
insert({K,V},T) -> insert(K,V,T).

%% @doc insert K := V into the Red Black Tree
-spec insert(K, V, redblacktree(K,V)) -> redBlackNode(K,V).
insert(X,VX,T) ->
  make_black(ins(X,VX,T)).

-spec make_black(redBlackNode(K,V)) -> redBlackNode(K,V).
make_black(T) -> T#rbTree{color=black}.

-spec ins(K, V, redblacktree(K,V)) -> redBlackNode(K,V).
ins(X,VX,empty) -> red(empty, X, VX, empty);
ins(X,VX,#rbTree{color=C, left=A, key=Y, value=VY, right=B}) when X < Y ->
  balance(C, ins(X,VX,A), Y,VY,B);
ins(X,VX,#rbTree{color=C, left=A, key=Y, value=VY, right=B}) when X > Y ->
  balance(C,A,Y,VY, ins(X,VX,B));
ins(X,VX,#rbTree{color=C, left=A, key=Y, value=_VY, right=B}) when X == Y ->
  node(C, A, X, VX, B).

balance(red, T1, K, VK, T2) -> red(T1, K, VK, T2);
balance(black,
    #rbTree{color=red, left=#rbTree{color=red, left=A, key=X, value=VX, right=B}, key=Y, value=VY, right=C},
    K, VK, T2) ->
  L = black(A, X, VX, B),
  R = black(C, K, VK, T2),
  red(L, Y, VY, R);
balance(black,
    #rbTree{color=red, left=A, key=X, value=VX, right=#rbTree{color=red, left=B, key=Y, value=VY, right=C}},
    K,VK,T2) ->
  L = black(A, X, VX, B),
  R = black(C, K, VK, T2),
  red(L, Y, VY, R);
balance(black, T1, K, VK,
    #rbTree{color=red, left=#rbTree{color=red, left=B, key=Y, value=VY, right=C}, key=Z, value=VZ, right=D}) ->
  L = black(T1, K, VK, B),
  R = black(C, Z, VZ, D),
  red(L, Y, VY, R);
balance(black, T1, K, VK, #rbTree{color=red, left=B, key=Y, value=VY, right=#rbTree{color=red, left=C, key=Z, value=VZ, right=D}}) ->
  red(black(T1, K, VK,B), Y, VY, black(C, Z, VZ, D));
balance(black,T1,K,VK,T2) -> black(T1, K, VK, T2).

%% @doc check if predicate P(K,V) is true for every pair in Red Black Tree
-spec forall(fun((K,V) -> boolean()), redblacktree(K,V)) -> boolean().
forall(_, empty) -> true;
forall(P, #rbTree{color=_C, left=L, key=K, value=V, right=R}) ->
  P(K,V) andalso forall(P, L) andalso forall(P, R).

%% @doc if tree is proper Red Black Tree
-spec is_red_black(redblacktree(_K,_V)) -> boolean().
is_red_black(empty) -> true;
is_red_black(#rbTree{left=empty, right=empty}) -> true;
is_red_black(#rbTree{color=_C, left=_L, key=_K, value=_V, right=_R}) ->
  false. %% TODO

%% @doc if tree is proper Red Black Tree
-spec is_bst(redblacktree(_K,_V)) -> boolean().
is_bst(empty) -> true;
is_bst(#rbTree{color=_C, left=L, key=K, value=_V, right=R}) ->
    forall(fun(K2,_) -> abs(K2) < abs(K) end, L)
    andalso forall(fun(K2,_) -> abs(K2) > abs(K) end, R)
    andalso is_bst(L)
    andalso is_bst(R).

%% @doc converts list to Red Black Tree
-spec from_list(list({K,V})) -> redblacktree(K,V).
from_list([]) -> empty();
from_list(XS) -> lists:foldl(fun insert/2, empty(), XS).

%% @doc converts Red Black Tree to association list - in order travrsal
-spec elements(redblacktree(K,V)) -> [{K,V}].
elements(T) -> elements(T, []).

elements(empty, Acc) -> Acc;
elements(#rbTree{color=_C, left=L, key=K, value=V, right=R}, Acc) ->
    elements(L, [{K,V} | elements(R, Acc)]).
