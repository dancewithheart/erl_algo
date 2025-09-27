-module(trie_tests).
-include_lib("eunit/include/eunit.hrl").

-define(NANI, ne).

trie_put_test_() ->
  [
    ?_assertEqual( {trie, ?NANI, #{} }, trie:new() ),
    ?_assertEqual(
      {trie, ?NANI, #{ $S =>
        {trie, cat, #{}}}},
      trie:put("S", cat, trie:new()) ),
    ?_assertEqual(
      {trie, ?NANI, #{ $S =>
        {trie, ?NANI, #{ $O =>
          {trie, cat, #{} }
      }}}},
      trie:put("SO", cat, trie:new()) ),
    ?_assertEqual(
      ?NANI,
      trie:get("n", trie:put("c", car, trie:new())) ),
    ?_assertEqual(
      car,
      trie:get("c", trie:put("c", car, trie:new())) ),
    ?_assertEqual(
      car,
      trie:get("car", trie:put("car", car, trie:new())) ),
    ?_assertEqual(
      ?NANI,
      trie:get("co", trie:put("c", car, trie:new())) )
  ].
