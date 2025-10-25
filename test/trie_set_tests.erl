-module(trie_set_tests).
-include_lib("eunit/include/eunit.hrl").
-import(trie_set, [member/2, new/0, add/2, from_list/1]).

trie_set_test_() ->
  [
    ?_assertEqual( false, member("foo", new()) ),
    ?_assertEqual( true, member("foo", add("foo", new()) ) )
  ].
