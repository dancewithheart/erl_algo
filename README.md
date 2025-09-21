erl_algo
===

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


Notes
---

* compile

```shell
rebar3 compile
```
* run unit tests

```shell
rebar3 eunit
```