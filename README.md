[![Erlang CI]](https://github.com/dancewithheart/erl_algo/actions/workflows/erlang.yml?query=branch%3Amain) [![GitHub last commit]](https://github.com/dancewithheart/erl_algo/commits/main/)

# erl_algo

Experiments with data structures and algorithms implemented in FP using Erlang:
* Binary Search Trees (BST) [src/bst.erl](./src/bst.erl), [test/prop_bst.erl](./test/prop_bst.erl)
* Red Black Trees [red_black_tree.erl](./src/red_black_tree.erl), [test/prop_red_black_tree.erl](./test/prop_red_black_tree.erl)
* Trie [trie.erl](./src/trie.erl), [test/prop_trie.erl](./test/prop_trie.erl)

## How [Rebar3]

* new property test (property names - , file - `test/prop_XYZ.erl`)
```shell
rebar3 new proper red_black_tree
```

* run [PropEr]ty tests

```shell
rebar3 proper
rebar3 proper -m prop_bst
```

* re-run property compilation / property tests on file change
```sh
find . -name '*.erl' | entr rebar3 compile
find . -name '*.erl' | entr rebar3 proper -m prop_red_black_tree
```

* static code analysis using [Dialyzer]

```shell
rebar3 dialyzer
```

* cross file analyse using [xref]:
```sh
rebar3 xref
```

* generate documentation ([EDoc])

```shell
rebar3 compile
```

* compile

```shell
rebar3 compile
```

* run [EUnit] tests

```shell
rebar3 eunit
rebar3 eunit --module=fib_tests
```

## Erlang and [Nix]

Nix Shell with Erlang 28

```shell
nix-shell -p erlang_28
```

[PropEr]: https://propertesting.com/book_stateless_properties.html
[Rebar3]: https://rebar3.org/docs/commands/
[xref]: https://www.erlang.org/doc/apps/tools/xref_chapter.html
[Dialyzer]: https://www.erlang.org/doc/apps/dialyzer/dialyzer_chapter.html
[EUnit]: https://www.erlang.org/doc/apps/eunit/chapter.html
[EDoc]: https://www.erlang.org/doc/apps/edoc/chapter.html
[Nix]: https://book.divnix.com/

[Erlang CI]: https://github.com/dancewithheart/erl_algo/actions/workflows/erlang.yml/badge.svg?branch=main
[GitHub last commit]: https://img.shields.io/github/last-commit/dancewithheart/erl_algo
