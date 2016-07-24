-module(dummy_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test/1]).
 
all() -> [test].

test(_Config) ->
  ok.
