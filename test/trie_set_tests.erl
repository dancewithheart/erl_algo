-module(trie_set_tests).
-include_lib("eunit/include/eunit.hrl").
-import(trie_set, [is_element/2, new/0, add_element/2, from_list/1]).

trie_set_test_() ->
  [
    ?_assertEqual( false, is_element("foo", new()) ),
    ?_assertEqual( true, is_element("foo", add_element("foo", new()) ) )
  ].
