# Why

Experiments with data structures implemented using FP language Erlang.
I am exploring how development focsing on the correctness and reliability would look like, based on my experience in Scala, Haskell, TypeScript, Rust, Scheme, Java and formal methods in Agda and Rocq. Hence:
* property tests, inspired by formal verification e.g. https://softwarefoundations.cis.upenn.edu/vfa-current/index.html
* unit tests
* use spec/types
* use of static code analysis

## How [Rebar3](https://rebar3.org/docs/)

* compile

```shell
rebar3 compile
```
* run [EUnit](https://www.erlang.org/doc/apps/eunit/chapter.html) tests

```shell
rebar3 eunit
rebar3 eunit --module=fib_tests
```

* run [PropEr](https://propertesting.com/toc.html)ty tests

```shell
rebar3 proper
rebar3 proper -m prop_bst
```

* static code analysis using [Dialyzer](https://www.erlang.org/doc/apps/dialyzer/dialyzer_chapter.html)

```shell
rebar3 dialyzer
```

## Erlang and [Nix](https://book.divnix.com/)

Nix Shell with Erlang 28

```shell
nix-shell -p erlang_28
```
