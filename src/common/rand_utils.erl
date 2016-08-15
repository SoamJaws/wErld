-module(rand_utils).
-include_lib("common_test/include/ct.hrl").

-export([uniform/2]).

-spec uniform(pos_integer(), pos_integer()) -> pos_integer().
uniform(Lo, Hi) ->
  crypto:rand_uniform(Lo, Hi).
