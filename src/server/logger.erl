-module(logger).
-compile(export_all).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

start_link(LogDir) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [LogDir], []).

stop(Module) ->
  gen_server:call(Module, stop).

stop() ->
  stop(?MODULE).

state(Module) ->
  gen_server:call(Module, state).

state() ->
  state(?MODULE).

init([LogDir]) ->
  {ok, LogDir}.

log_info(Content, Module, Id) ->
  log(Content, Module, Id, "INFO").

log_warning(Content, Module, Id) ->
  log(Content, Module, Id, "WARNING").

log_error(Content, Module, Id) ->
  log(Content, Module, Id, "ERROR").

log(Content, Module, Id, Mode) ->
  {_Date, {H, M, S}} = calendar:local_time(),
  gen_server:cast(?MODULE, {log, Module, Id, io_lib:fwrite("--- ~s ~s:~s:~s - ~s --- ~s~n--- END ~s ---~n", [Mode, H, M, S, self(), Content, Mode])}).

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State}.

handle_cast({log, Module, Id, Content}, LogDir) ->
  LogFile = filename:join([LogDir, Module, Id ++ ".log"]),
  file:write_file(LogFile, Content, [append]),
  {noreply, LogDir}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
