-module(logger).
-behaviour(gen_server).
-include("gen_server_utils.hrl").

-define(COMPOSITE_LOG(LogDir), filename:join([LogDir, "composite_log.html"])).
-define(COMPOSITE_DIAGRAM(LogDir), filename:join([LogDir, "composite_diagram.txt"])).

-export([ log_info/3
        , log_warning/3
        , log_error/3
        , log_send/5
        , log_receive/3
        , log_receive/5
        , stop/0]).

-export([ start_link/1
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).


%% Public API

-spec log_info(string(), string(), atom()) -> ok.
log_info(Content, Module, Id) ->
  log(Content, Module, Id, "INFO").

-spec log_warning(string(), string(), atom()) -> ok.
log_warning(Content, Module, Id) ->
  log(Content, Module, Id, "WARNING").

-spec log_error(string(), string(), atom()) -> ok.
log_error(Content, Module, Id) ->
  log(Content, Module, Id, "ERROR").

-spec log_send(string(), string(), atom(), gen_address(), gen_address()) -> ok.
log_send(Content, Module, Id, From, To) ->
  log(Content, Module, Id, "SEND"),
  log_sig(Content, From, To).

-spec log_receive(string(), string(), atom()) -> ok.
log_receive(Content, Module, Id) ->
  log(Content, Module, Id, "RECEIVE").

-spec log_receive(string(), string(), atom(), gen_address(), gen_address()) -> ok.
log_receive(Content, Module, Id, From, To) ->
  log(Content, Module, Id, "RECEIVE"),
  log_sig(Content, From, To).

-spec stop() -> ok.
stop() ->
  gen_server:call({global, ?MODULE}, stop).


%% Gen server

start_link(LogDir) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [LogDir], []).


init([LogDir]) ->
  UpdatedLogDir = filename:join(["logs", LogDir]),
  filelib:ensure_dir(?COMPOSITE_LOG(UpdatedLogDir)),
  file:write_file(?COMPOSITE_DIAGRAM(UpdatedLogDir), "@startuml\n"),
  {ok, UpdatedLogDir}.


-spec handle_call(stop, {pid(), any()}, any()) -> {stop, normal, stopped, any()}.
handle_call(stop, _From, LogDir) ->
  file:write_file(?COMPOSITE_DIAGRAM(LogDir), "@enduml", [append]),
  {stop, normal, stopped, LogDir};

handle_call(_Call, _From, LogDir) ->
  {reply, undefined, LogDir}.


-spec handle_cast({log_sig, string(), atom(), string(), gen_address(), gen_address()}, string()) -> {noreply, string()};
                 ({log, string(), atom(), string()}, string()) -> {noreply, string()}.
handle_cast({log_sig, Content, {{FromModule, FromId}, _FromPid}, {{ToModule, ToId}, _ToPid}}, LogDir) ->
  file:write_file(?COMPOSITE_DIAGRAM(LogDir), io_lib:format("~s~n", [re:replace(re:replace(io_lib:fwrite("~s_~s->~s_~s : ~s", [FromModule, FromId, ToModule, ToId, Content]), "\\n+", "", [global, {return,list}]), "(\\s+\\s+)", " ", [global,{return,list}])]), [append]),
  {noreply, LogDir};

handle_cast({log, Module, Id, Content}, LogDir) ->
  LogFile = filename:join([LogDir, Module ++ "_" ++ atom_to_list(Id) ++ "_log.html"]),
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


-spec log_sig(string(), gen_address(), gen_address()) -> ok.
log_sig(Content, From, To) ->
  gen_server:cast({global, ?MODULE}, {log_sig, Content, From, To}).

-spec log(string(), string(), atom(), string()) -> ok.
log(Content, Module, Id, Mode) ->
  {_Date, {H, M, S}} = calendar:local_time(),
  Header = io_lib:fwrite("--- ~s --- ~w:~w:~w ~s - ~w", [Mode, H, M, S, Module, self()]),
  gen_server:cast({global, ?MODULE}, {log, Module, Id, io_lib:fwrite("~s~n~n    ~s~n~n~s~n~n", [Header, Content, lists:duplicate(length(lists:flatten(Header)), $-)])}).
