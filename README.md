# Erlang stuff

## Notes on [Rebar3](https://rebar3.org/docs/)

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
rebar3 proper -m bst_tests
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
