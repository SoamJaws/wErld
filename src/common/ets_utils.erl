-module(ets_utils).

-export([set_lookup/2]).

-spec set_lookup(atom(), atom()) -> any().
set_lookup(TableName, Key) ->
  [{Key, Value}] = ets:lookup(TableName, Key),
  Value.
