%%====================================================================
%%
%% @author Johan Wikström Schützer <johan.dss@gmail.com>
%% @copyright 2015 DrippingBits
%%
%%
%%
%%====================================================================
-module(blackboard).
-compile(export_all).

-include("blackboard.hrl").
%TODO include nameserver.hrl, add nameserver name mactro to nameserver.hrl

-behaviour(gen_server).
-export([start_link/1, stop/0, subscribe/2, unsubscribe/2, post/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Public API
%%====================================================================

start_link(Name) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Name], []).

stop() ->
  stop(?MODULE).

subscribe(BlackboardPid, Tag) ->
  gen_server:call(BlackboardPid, {subscribe, Tag}).

unsubscribe(BlackboardPid, Tag) ->
  gen_server:call(BlackboardPid, {unsubscribe, Tag}).

post(BlackboardPid, Tag, Content) ->
  gen_server:call(BlackboardPid, {post, Tag, Content}).

%%TODO Think about how to use locally registered names

%%====================================================================
%% Internal functions
%%====================================================================

stop(Module) ->
  gen_server:call(Module, stop).

addSubscriber(Tag, From, []) ->
  [{Tag, [From]}];
addSubscriber(Tag, From, [{Tag, Pids}|T]) ->
  [{Tag, [From|Pids]}|T];
addSubscriber(Tag, From, [H|T]) ->
  H:addSubscriber(Tag, From, T).

removeSubscriber(_Tag, _From, []) -> [];
removeSubscriber(Tag, From, [{Tag, Pids}|T]) ->
  [{Tag,lists:delete(From,Pids)}|T];
removeSubscriber(Tag, From, [H|T]) ->
  [H|removeSubscriber(Tag, From, T)].

broadCast(_Tag, _Content, []) -> ok;
broadCast(Tag, Content, [{Tag, Pids}|_T]) ->
  lists:map(fun(Pid) -> gen_server:cast(Pid, Content) end, Pids);
broadCast(Tag, Content, [_H|T]) ->
  broadCast(Tag, Content, T).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Name]) ->
  %TODO use nameserver macro instead of hardcoded name
  FullName = {?BLACKBOARD_PREFIX, Name},
  gen_server:call(nameserver, {publish, FullName, self()}),
  {ok, []}.


handle_call(stop, _From, Subscriptions) ->
  {stop, normal, stopped, Subscriptions};

handle_call({subscribe, Tag}, From, Subscriptions) ->
  UpdatedSubscribers = addSubscriber(Tag, From, Subscriptions),
  {noreply, UpdatedSubscribers};

handle_call({unsubscribe, Tag}, From, Subscriptions) ->
  UpdatedSubscribers = removeSubscriber(Tag, From, Subscriptions),
  {noreply, UpdatedSubscribers};

handle_call({post, Tag, Content}, _From, Subscriptions) ->
  broadCast(Tag, Content, Subscriptions),
  {noreply, Subscriptions}.


handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.