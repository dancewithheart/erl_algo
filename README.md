[![Erlang CI]](https://github.com/dancewithheart/erl_algo/actions/workflows/erlang.yml?query=branch%3Amain) [![GitHub last commit]](https://github.com/dancewithheart/erl_algo/commits/main/)

# Why

Experiments with data structures and algorithms implemented in FP language - Erlang.

I am exploring how development focusing on the correctness and reliability looks like, based on my experience in Scala, Haskell and formal verification in Agda and Rocq:
* property tests, inspired by formal verification e.g. https://softwarefoundations.cis.upenn.edu/vfa-current/index.html
* use spec/types
* use of static code analysis
* unit tests

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
