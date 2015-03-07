-module(blackboard).
-compile(export_all).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

start() ->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

stop(Module) ->
  gen_server:call(Module, stop).

stop() ->
  stop(?MODULE).

state(Module) ->
  gen_server:call(Module, state).

state() ->
  state(?MODULE).

init([]) ->
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
