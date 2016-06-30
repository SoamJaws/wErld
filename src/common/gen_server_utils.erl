-module(gen_server_utils).

-export([ cast/3
        , notify_caller/2]).

cast(Pid, Msg, BlockCaller) ->
  UpdatedMsg = if
                 is_tuple(Msg) ->
                   lists:foldl(fun(V, T) -> erlang:insert_element(tuple_size(T) + 1, T, V) end, Msg, [BlockCaller, self()]);
                 true ->
                   {Msg, BlockCaller, self()}
               end,
  gen_server:cast(Pid, UpdatedMsg),
  block_caller(BlockCaller).

handle_cast(Msg, State, Fun) ->
  %% TODO Extract NotifyCaller and Caller
  %% Call Fun(State, RemainingTuple)  and save new state
  %% Notify Caller
  %% Return state

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
