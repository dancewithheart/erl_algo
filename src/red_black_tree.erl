%% @doc Red Black Tree implemented based on
%% Software Foundations, Volume 3, Verified Functional Algorithms
%% Andrew W. Appel
%% https://softwarefoundations.cis.upenn.edu/vfa-current/Redblack.html
%% Red Black Tree is self-balancing binary search tree, each node has color
-module(red_black_tree).
-export([get/3, put/2, put/3,
    is_empty/1, is_leaf/1, is_key/2,
    all/2,
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
-spec is_key(K, redblacktree(K,_V)) -> boolean().
is_key(_, empty) -> false;
is_key(X, #rbTree{left=L, key=K}) when X < K -> is_key(X,L);
is_key(X, #rbTree{key=K, right=R}) when X > K -> is_key(X,R);
is_key(_, _) -> true.

%% @doc get value is_key to K in Red Black Tree, or default value D
-spec get(V, K, redblacktree(K,V)) -> V.
get(D, _X, empty) -> D;
get(D, X, #rbTree{color=_C, left=L, key=K, value=_V, right=_R}) when X < K -> get(D,X,L);
get(D, X, #rbTree{color=_C, left=_L, key=K, value=_V, right=R}) when X > K -> get(D,X,R);
get(_D, _X, #rbTree{color=_C, left=_L, key=_K, value=V, right=_R}) -> V.

%% @doc put K := V into the Red Black Tree
-spec put({K, V}, redblacktree(K,V)) -> redBlackNode(K,V).
put({K,V},T) -> put(K,V,T).

%% @doc put K := V into the Red Black Tree
-spec put(K, V, redblacktree(K,V)) -> redBlackNode(K,V).
put(X,VX,T) ->
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
-spec all(fun((K,V) -> boolean()), redblacktree(K,V)) -> boolean().
all(_, empty) -> true;
all(P, #rbTree{color=_C, left=L, key=K, value=V, right=R}) ->
  P(K,V) andalso all(P, L) andalso all(P, R).

%% @doc true if this is Red Black Tree
%% Red Black Tree have to maintain invariant
%% * Root is black.
%% * No red node has a red child.
%% * TODO Every path from the root to a leaf has the same number of black nodes.
-spec is_red_black(redblacktree(_K,_V)) -> boolean().
is_red_black(L) ->
  root_is_black(L) andalso
  red_has_no_red_child(L) andalso
  is_bst(L).

root_is_black(#rbTree{color=red}) -> false;
root_is_black(_) -> true.

red_has_no_red_child(#rbTree{color=red, left=L}) when L#rbTree.color == red -> false;
red_has_no_red_child(#rbTree{color=red, right=R}) when R#rbTree.color == red -> false;
red_has_no_red_child(empty) -> true;
red_has_no_red_child(#rbTree{left=L, right=R}) ->
  red_has_no_red_child(L) andalso red_has_no_red_child(R).

%% @doc if tree is proper Red Black Tree
-spec is_bst(redblacktree(_K,_V)) -> boolean().
is_bst(empty) -> true;
is_bst(#rbTree{left=L, key=K, right=R}) ->
  all(fun(K2,_) -> K2 < K end, L) andalso
  all(fun(K2,_) -> K2 > K end, R) andalso
  is_bst(L) andalso
  is_bst(R).

%% @doc converts list to Red Black Tree
-spec from_list(list({K,V})) -> redblacktree(K,V).
from_list(T) -> lists:foldl(fun put/2, empty(), T).

%% @doc converts Red Black Tree to association list - in order travrsal
-spec elements(redblacktree(K,V)) -> [{K,V}].
elements(T) -> elements(T, []).

elements(empty, Acc) -> Acc;
elements(#rbTree{color=_C, left=L, key=K, value=V, right=R}, Acc) ->
    elements(L, [{K,V} | elements(R, Acc)]).
