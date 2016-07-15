-module(gen_server_utils).
-include("gen_server_utils.hrl").

-export([ cast/3
        , notify_caller/2
        , extract_module/1]).

-spec cast(pid(), tuple(), boolean()) -> ok.
cast(Pid, Msg, BlockCaller) ->
  UpdatedMsg = if
                 is_tuple(Msg) ->
                   lists:foldl(fun(V, T) -> erlang:insert_element(tuple_size(T) + 1, T, V) end, Msg, [BlockCaller, self()]);
                 true ->
                   {Msg, BlockCaller, self()}
               end,
  gen_server:cast(Pid, UpdatedMsg),
  block_caller(BlockCaller).

-spec block_caller(boolean()) -> ok.
block_caller(BlockCaller) ->
  if
    BlockCaller ->
      receive
        ok -> ok
      end;
    true ->
      ok
  end.

-spec notify_caller(boolean(), pid()) -> ok.
notify_caller(NotifyCaller, Caller) ->
  if
    NotifyCaller ->
      Caller ! ok;
    true ->
      ok
  end.

-spec extract_module(gen_address()) -> atom().
extract_module({{Module, _Id}, _Pid}) -> Module.
