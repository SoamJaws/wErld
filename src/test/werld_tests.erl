-module(werld_tests).
-include_lib("eunit/include/eunit.hrl").

werld_test() ->
  eunit:test([client_tests], [verbose]).
