%%====================================================================
%%
%% @author Johan Wikström Schützer <johan.dss@gmail.com>
%% @copyright 2015 DrippingBits
%%
%% This is the global nameserver. It is a globally registered process
%% used for looking up processes by name. Any process may subscribe
%% to any name in the nameserver if it has a name to lookup. Any
%% process that should be published in the nameserver should publish
%% itself.
%%
%%====================================================================
-module(nameserver).
-compile(export_all).

-behaviour(gen_server).
-export([start_link/0, stop/0, subscribe/1, unsubscribe/1, publish/2, unpublish/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Public API
%%====================================================================

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
  stop(?MODULE).

subscribe(Name) ->
  gen_server:call(?MODULE, {subscribe, Name}).

unsubscribe(Name) ->
  gen_server:call(?MODULE, {unsubscribe, Name}).

publish(Name, Pid) ->
  gen_server:call(?MODULE, {publish, Name, Pid}).

unpublish(Name) ->
  gen_server:call(?MODULE, {unpublish, Name}).

%%====================================================================
%% Internal functions
%%====================================================================

stop(Module) ->
  gen_server:call(Module, stop).

notifyPublished(_Name, _Pid, []) -> ok;
notifyPublished(Name, Pid, [H|T]) ->
  gen_server:cast(H, {published, Name, Pid}),
  notifyPublished(Name, Pid, T).

notifyUnpublished(_Name, []) -> ok;
notifyUnpublished(Name, [H|T]) ->
  gen_server:cast(H, {unpublished, Name}),
  notifyUnpublished(Name, T).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
  {ok, {dict:new(), dict:new()}}.

handle_call(stop, _From, {Publications, Subscribers}) ->
  {stop, normal, stopped, {Publications, Subscribers}};

handle_call({subscribe, Name}, From, {Publications, Subscribers}) ->
  UpdatedSubscribers = dict:append(Name, From, Subscribers),
  NamePublished = dict:is_key(Name, Publications),
  if
    NamePublished ->
      Pid = dict:fetch(Name, Publications),
      notifyPublished(Name, Pid, [From])
  end,
  {noreply, {Publications, UpdatedSubscribers}};

handle_call({unsubscribe, Name}, _From, {Publications, Subscribers}) ->
  UpdatedSubscribers = dict:erase(Name, Subscribers),
  {noreply, {Publications, UpdatedSubscribers}};

handle_call({publish, Name, Pid}, _From, {Publications, Subscribers}) ->
  notifyPublished(Name, Pid, dict:fetch(Name, Subscribers)),
  UpdatedPublications = dict:store(Name, Pid, Publications),
  {noreply, {UpdatedPublications, Subscribers}};

handle_call({unpublish, Name}, _From, {Publications, Subscribers}) ->
  notifyUnpublished(Name, dict:fetch(Name, Subscribers)),
  UpdatedPublications = dict:erase(Name, Publications),
  {noreply, {UpdatedPublications, Subscribers}}.


handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
