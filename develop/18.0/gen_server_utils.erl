-module(gen_server_utils).

-export([ block_caller/1
        , notify_caller/2]).

block_caller(BlockCaller) ->
  if
    BlockCaller ->
      receive
        done -> ok
      end;
    true ->
      ok
  end.

notify_caller(NotifyCaller, Caller) ->
  if
    NotifyCaller ->
      Caller ! done;
    true ->
      ok
  end.
