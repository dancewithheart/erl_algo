# Erlang stuff

```text
 _________________________________________
/ Hadamard gate and CNOT gate transform 2 \
| qubits so they become entangled         |
|                                         |
| |00⟩ + |11⟩                             |
|                                         |
| -----------                             |
|                                         |
| sqrt(2)                                 |
|                                         |
| If you measure first as 0 the second    |
\ have to be 0.                           /
 -----------------------------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||

```
[Bell state](https://en.wikipedia.org/wiki/Bell_state)


## Notes on [rebar3](https://rebar3.org/docs/)


* compile

```shell
rebar3 compile
```
* run [eunit](https://www.erlang.org/doc/apps/eunit/chapter.html) tests

```shell
rebar3 eunit
rebar3 eunit --test=fib_tests:fib_test_
```

* static code analysis using [Dialyzer](https://www.erlang.org/doc/apps/dialyzer/dialyzer_chapter.html)

```shell
rebar3 dialyzer
```

## Other Erlang notes

* new project (app has supervisor)
```sh
rebar3 new lib erl_algo
```

Notes on erl
----

* erl --version :scream: :
```sh
erl +V
python3 --version
ghc --version
rustc --version
nix --version
```

* analyse code using Dialyzer

(You need to initialize Dialyzer onc per Erlang installation:
```shell
dialyzer --build_plt --apps erts kernel stdlib
```
)

```shell
dialyzer src/fib.erl
dialyzer src/*.erl
```