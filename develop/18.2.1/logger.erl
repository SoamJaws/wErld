-module(logger).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).

-define(COMPOSITE_LOG(LogDir), filename:join([LogDir, "composite_log"])).

-export([ log_info/4
        , log_warning/4
        , log_error/4
        , log_send/4
        , log_receive/4]).

-export([ start_link/1
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).


%% Public API

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


%% Gen server

start_link(LogDir) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [LogDir], []).


init([LogDir]) ->
  filelib:ensure_dir(?COMPOSITE_LOG(LogDir)),
  {ok, LogDir}.


handle_call(_Call, _From, State) ->
  {reply, undefined, State}.


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


%% Helpers

-spec log(string(), string(), pos_integer(), atom(), string()) -> ok.
log(Content, Module, Line, Id, Mode) ->
  {_Date, {H, M, S}} = calendar:local_time(),
  Header = io_lib:fwrite("--- ~s --- ~w:~w:~w ~s:~w - ~w", [Mode, H, M, S, Module, Line, self()]),
  gen_server:cast({global, ?MODULE}, {log, Module, Id, io_lib:fwrite("~s~n~n    ~s~n~n~s~n~n", [Header, Content, lists:duplicate(length(lists:flatten(Header)), $-)])}).

