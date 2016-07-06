-module(logger).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(COMPOSITE_LOG(LogDir), filename:join([LogDir, "composite_log"])).

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
  filelib:ensure_dir(?COMPOSITE_LOG(LogDir)),
  {ok, LogDir}.

-spec log_info(string(), string(), pos_integer(), atom()) -> ok.
log_info(Content, Module, Line, Id) ->
  log(Content, Module, Line, Id, "INFO").

-spec log_warning(string(), string(), pos_integer(), atom()) -> ok.
log_warning(Content, Module, Line, Id) ->
  log(Content, Module, Line, Id, "WARNING").

-spec log_error(string(), string(), pos_integer(), atom()) -> ok.
log_error(Content, Module, Line, Id) ->
  log(Content, Module, Line, Id, "ERROR").

-spec log_send(string(), string(), pos_integer(), atom()) -> ok.
log_send(Content, Module, Line, Id) ->
  log(Content, Module, Line, Id, "SEND").

-spec log_receive(string(), string(), pos_integer(), atom()) -> ok.
log_receive(Content, Module, Line, Id) ->
  log(Content, Module, Line, Id, "RECEIVE").

-spec log(string(), string(), pos_integer(), atom(), string()) -> ok.
log(Content, Module, Line, Id, Mode) ->
  {_Date, {H, M, S}} = calendar:local_time(),
  Header = io_lib:fwrite("--- ~s --- ~w:~w:~w ~s:~w - ~w", [Mode, H, M, S, Module, Line, self()]),
  gen_server:cast({global, ?MODULE}, {log, Module, Id, io_lib:fwrite("~s~n~n    ~s~n~n~s~n~n", [Header, Content, lists:duplicate(length(lists:flatten(Header)), $-)])}).

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State}.

-spec handle_cast({log, string(), atom(), string()}, string()) -> {noreply, string()}.
handle_cast({log, Module, Id, Content}, LogDir) ->
  LogFile = filename:join([LogDir, Module ++ "_" ++ atom_to_list(Id) ++ ".log"]),
  file:write_file(LogFile, Content, [append]),
  file:write_file(?COMPOSITE_LOG(LogDir), Content, [append]),
  {noreply, LogDir}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
