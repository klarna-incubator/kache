%% @doc A generic, in-memory caching application based on ETS.
-module(kache).
-behaviour(gen_server).

-define(DEFAULT_TIMEOUT, 5000).

-type ttl() :: {erlang:time_unit(), integer()} | infinity.
%% Time-to-live for individual cache entries.

-type generator() :: fun(() -> term()) | {module(), atom(), [term()]}.
%% A generator function for a value to be cached.  Runs in its own
%% process and must complete within the timeout given to `get_fill'.

-type eviction() :: none | fifo | lru | random | module().
%% Cache eviction strategy.  Either a built-in strategy or a module
%% implementing the `kache_eviction' behavior.

-type option() ::
        {eviction, eviction()}
      | {capacity, pos_integer() | infinity}
      | compressed.
%% Cache configuration options.

-type cache() :: pid() | atom().
%% A cache handle, returned from `start_link'.

-type info() ::
        {size, non_neg_integer()}
      | {memory, non_neg_integer()}.
%% Informational items returned by `info'.

-export_type([ ttl/0
             , option/0
             , info/0
             , cache/0
             ]).

-export([ start_link/1
        , start_link/2
        , stop/1
        , put/3
        , put/4
        , put/5
        , get/2
        , get/3
        , get_wait/2
        , get_wait/3
        , get_fill/3
        , get_fill/4
        , get_fill/5
        , remove/2
        , remove/3
        , sweep/1
        , sweep/2
        , evict/1
        , evict/2
        , evict/3
        , purge/1
        , purge/2
        , info/1
        , info/2
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

%% @doc Create a new cache, linked to the calling process.
%%
%% Accepted options are `{eviction, eviction()}' to select the
%% eviction strategy, `{capacity, pos_integer() | infinity}' to set
%% the cache capacity, and `compressed' to store data in a compressed
%% ETS table.
-spec start_link([option()]) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
  gen_server:start_link(?MODULE, Options, []).

%% @doc Create a new cache and register the process locally or
%% globally with the given name.
%%
%% The name can be used as a cache handle for `get', `put', `remove',
%% etc.
%%
%% @see start_link/1
-spec start_link(ServerName, [option()]) -> {ok, pid()} | {error, term()}
          when ServerName :: {local, atom()} | {global, atom()}.
start_link(ServerName, Options) ->
  gen_server:start_link(ServerName, ?MODULE, Options, []).

%% @doc Stop and remove a cache.
-spec stop(cache()) -> ok.
stop(Cache) ->
  gen_server:stop(Cache).

%% @equiv put(Cache, Key, Value, infinity, DEFAULT_TIMEOUT)
-spec put(cache(), term(), term()) -> ok.
put(Cache, Key, Value) ->
  put(Cache, Key, Value, infinity).

%% @equiv put(Cache, Key, Value, Ttl, DEFAULT_TIMEOUT)
-spec put(cache(), term(), term(), ttl()) -> ok.
put(Cache, Key, Value, Ttl) ->
  put(Cache, Key, Value, Ttl, ?DEFAULT_TIMEOUT).

%% @doc Put an item into the cache, replacing any existing item with
%% the same `Key'.
%%
%% This will make any waiting `get_wait' or `get_fill' calls return
%% `{ok, Value}'.
-spec put(cache(), term(), term(), ttl(), timeout()) -> ok.
put(Cache, Key, Value, Ttl, Timeout) ->
  gen_server:call(Cache, {put, Key, Value, Ttl}, Timeout).

%% @equiv get(Cache, Key, DEFAULT_TIMEOUT)
-spec get(cache(), term()) -> {ok, term()} | notfound.
get(Cache, Key) ->
  get(Cache, Key, ?DEFAULT_TIMEOUT).

%% @doc Retrieve an item from the cache, or return `notfound' the item
%% does not exist.
-spec get(cache(), term(), timeout()) -> {ok, term()} | notfound.
get(Cache, Key, Timeout) ->
  gen_server:call(Cache, {get, Key}, Timeout).

%% @equiv get_wait(Cache, Key, DEFAULT_TIMEOUT)
-spec get_wait(cache(), term()) -> {ok, term()} | notfound.
get_wait(Cache, Key) ->
  get_wait(Cache, Key, ?DEFAULT_TIMEOUT).

%% @doc Retrieve an item from the cache, waiting if the item to be
%% `put' or `remove'd if it does not exist.
-spec get_wait(cache(), term(), timeout()) -> {ok, term()} | notfound.
get_wait(Cache, Key, Timeout) ->
  gen_server:call(Cache, {get_wait, Key}, Timeout).

%% @equiv get_fill(Cache, Key, Generator, infinity, DEFAULT_TIMEOUT)
-spec get_fill(cache(), term(), generator()) -> {ok, term()} | notfound.
get_fill(Cache, Key, Generator) ->
  get_fill(Cache, Key, Generator, infinity).

%% @equiv get_fill(Cache, Key, Generator, Ttl, DEFAULT_TIMEOUT)
-spec get_fill(cache(), term(), generator(), ttl()) -> {ok, term()} | notfound.
get_fill(Cache, Key, Generator, Ttl) ->
  get_fill(Cache, Key, Generator, Ttl, ?DEFAULT_TIMEOUT).

%% @doc Retrieve an item from the cache, or generate it if does not
%% exist.
%%
%% Only the first call of `get_fill' on a given `Key' calls
%% `Generator'.  Any subsequent calls behave just like
%% `get_wait(Cache, Key, Timeout)'.
-spec get_fill(cache(), term(), generator(), ttl(), timeout()) -> {ok, term()} | notfound.
get_fill(Cache, Key, {Module, Function, Args}, Ttl, Timeout) ->
  get_fill(Cache, Key, fun() -> apply(Module, Function, Args) end, Ttl, Timeout);
get_fill(Cache, Key, Generator, Ttl, Timeout) ->
  gen_server:call(Cache, {get_fill, Key, Generator, Ttl, Timeout}, Timeout).

%% @equiv remove(Cache, Key, DEFAULT_TIMEOUT)
-spec remove(cache(), term()) -> ok.
remove(Cache, Key) ->
  remove(Cache, Key, ?DEFAULT_TIMEOUT).

%% @doc Remove an item from the Cache.
%%
%% This will make any waiting `get_wait' or `get_fill' calls return
%% `notfound'.
-spec remove(cache(), term(), timeout()) -> ok.
remove(Cache, Key, Timeout) ->
  gen_server:call(Cache, {remove, Key}, Timeout).

%% @equiv sweep(Cache, DEFAULT_TIMEOUT)
-spec sweep(cache()) -> non_neg_integer().
sweep(Cache) ->
  sweep(Cache, ?DEFAULT_TIMEOUT).

%% @doc Remove all expired entries from the cache.
-spec sweep(cache(), timeout()) -> non_neg_integer().
sweep(Cache, Timeout) ->
  gen_server:call(Cache, sweep, Timeout).

%% @equiv evict(Cache, 0, DEFAULT_TIMEOUT)
-spec evict(cache()) -> ok.
evict(Cache) ->
  evict(Cache, 0).

%% @equiv evict(Cache, N, DEFAULT_TIMEOUT)
-spec evict(cache(), non_neg_integer()) -> ok.
evict(Cache, N) ->
  evict(Cache, N, ?DEFAULT_TIMEOUT).

%% @doc Ask the eviction strategy to remove at least `N' items from
%% the cache.
-spec evict(cache(), non_neg_integer(), timeout()) -> ok.
evict(Cache, N, Timeout) ->
  gen_server:call(Cache, {evict, N}, Timeout).

%% @equiv purge(Cache, DEFAULT_TIMEOUT)
-spec purge(cache()) -> ok.
purge(Cache) ->
  purge(Cache, ?DEFAULT_TIMEOUT).

%% @doc Empty the cache
-spec purge(cache(), timeout()) -> ok.
purge(Cache, Timeout) ->
  gen_server:call(Cache, purge, Timeout).

%% @equiv info(Cache, DEFAULT_TIMEOUT)
-spec info(cache()) -> [info()].
info(Cache) ->
  info(Cache, ?DEFAULT_TIMEOUT).

%% @doc Return size and memory consumption of the cache
-spec info(cache(), timeout()) -> [info()].
info(Cache, Timeout) ->
  gen_server:call(Cache, info, Timeout).

%% gen_server

-record(state,
        { table    :: ets:tid()
        , eviction :: kache_eviction:strategy()
        , capacity :: pos_integer()
        , waiting  :: ets:tid()
        }).

%% @private
init(Options) ->
  EtsOptions = lists:flatmap(fun ets_option/1, Options),
  Table = ets:new(?MODULE, [set | EtsOptions]),
  Eviction = proplists:get_value(eviction, Options, none),
  Capacity = proplists:get_value(capacity, Options, infinity),
  Waiting = ets:new(?MODULE, [bag]),
  State = #state{ table = Table
                , eviction = kache_eviction:new(eviction_module(Eviction))
                , capacity = Capacity
                , waiting = Waiting
                },
  {ok, State}.

-spec ets_option(option()) -> [term()].
ets_option(compressed) ->
  [compressed];
ets_option(_) ->
  [].

-spec eviction_module(eviction()) -> module().
eviction_module(Eviction) ->
  case Eviction of
    none ->
      kache_ev_none;
    fifo ->
      kache_ev_fifo;
    lru ->
      kache_ev_lru;
    random ->
      kache_ev_random;
    _ ->
      Eviction
  end.

%% @private
handle_call({put, Key, Value, Ttl}, _, State) ->
  do_put(Key, Value, Ttl, State);
handle_call({get, Key}, _, State) ->
  do_get(Key, State);
handle_call({get_wait, Key}, From, State) ->
  do_get_wait(Key, From, State);
handle_call({get_fill, Key, Generator, Ttl, Timeout}, From, State) ->
  do_get_fill(Key, Generator, Ttl, Timeout, From, State);
handle_call({remove, Key}, _, State) ->
  do_remove(Key, State);
handle_call(sweep, _, State) ->
  do_sweep(State);
handle_call({evict, N}, _, State) ->
  do_evict(N, State);
handle_call(purge, _, State) ->
  do_purge(State);
handle_call(info, _, State) ->
  do_info(State);
handle_call(_, _, State) ->
  {noreply, State}.

%% @private
handle_cast(_, State) ->
  {noreply, State}.

do_put(Key, Value, Ttl, State0) ->
  #state{ table = Table, capacity = Capacity } = State0,
  case ets:lookup(Table, Key) of
    [{Key, _, _, EvKey}] ->
      State1 = cache_remove(Key, EvKey, State0);
    _ ->
      State1 = State0
  end,
  Expire = to_expire(Ttl),
  State2 = cache_insert(Key, Value, Expire, State1),
  State3 = cache_notify(Key, {ok, Value}, State2),
  Size = ets:info(Table, size),
  case Capacity =/= infinity andalso Size > Capacity of
    true ->
      N = Size - Capacity,
      do_evict(N, State3);
    false ->
      {reply, ok, State3}
  end.

do_get(Key, State0) ->
  {Response, State} = cache_get(Key, State0),
  {reply, Response, State}.

do_get_wait(Key, From, State0) ->
  {Response, State1} = cache_get(Key, State0),
  case Response of
    {ok, _} ->
      {reply, Response, State1};
    notfound ->
      State = cache_wait(Key, From, State1),
      {noreply, State}
  end.

do_get_fill(Key, Generator, Ttl, Timeout, From, State0) ->
  {Response, State1} = cache_get(Key, State0),
  case Response of
    {ok, _} ->
      {reply, Response, State1};
    _ ->
      GeneratorRunning = ets:member(State1#state.waiting, Key),
      State2 = cache_wait(Key, From, State1),
      case GeneratorRunning of
        true ->
          {noreply, State2};
        false ->
          Cache = self(),
          spawn(fun() -> generate_and_insert(Cache, Key, Generator, Ttl, Timeout) end),
          {noreply, State2}
      end
  end.

do_remove(Key, State0) ->
  case ets:lookup(State0#state.table, Key) of
    [{Key, _, _, EvKey}] ->
      State1 = cache_remove(Key, EvKey, State0);
    _ ->
      State1 = State0
  end,
  State = cache_notify(Key, notfound, State1),
  {reply, ok, State}.

do_sweep(State0) ->
  Now = erlang:monotonic_time(),
  Results =
    ets:select(State0#state.table,
               [{ {'_', '_', '$1', '$2'}
                , [{'andalso',
                    {'=/=', '$1', infinity},
                    {'<', '$1', {const, Now}}}]
                , [{{'$1', '$2'}}]
                }]),
  State = lists:foldl(fun({Key, EvKey}, S) -> cache_remove(Key, EvKey, S) end,
                      State0,
                      Results),
  {reply, length(Results), State}.

do_evict(N, State0) ->
  Results = kache_eviction:evict(State0#state.eviction, N),
  State = lists:foldl(fun({EvKey, Key}, S) -> cache_remove(Key, EvKey, S) end,
                      State0,
                      Results),
  {reply, ok, State}.

do_purge(State0) ->
  State = cache_purge(State0),
  {reply, ok, State}.

do_info(State) ->
  Info = ets:info(State#state.table),
  Size = proplists:lookup(size, Info),
  Memory = proplists:lookup(memory, Info),
  {reply, [Size, Memory], State}.

generate_and_insert(Cache, Key, Generator, Ttl, Timeout) ->
  {Pid, Monitor} = spawn_monitor(exit_with(Generator)),
  {ok, Timer} = timer:exit_after(Timeout, Pid, timeout),
  receive
    {'DOWN', Monitor, process, Pid, Result} -> ok
  end,
  {ok, cancel} = timer:cancel(Timer),
  case Result of
    {ok, Value} ->
      put(Cache, Key, Value, Ttl);
    _ ->
      remove(Cache, Key)
  end.

-spec exit_with(fun(() -> term())) -> fun(() -> no_return()).
exit_with(Generator) ->
  fun () ->
      Response =
        try Generator() of
          Result ->
            {ok, Result}
        catch
          Type:Reason ->
            {Type, Reason}
        end,
      exit(Response)
  end.

cache_get(Key, State0) ->
  case ets:lookup(State0#state.table, Key) of
    [{Key, Value, Expire, EvKey}] ->
      case is_expired(Expire) of
        false ->
          State = cache_touch(Key, EvKey, State0),
          Response = {ok, Value};
        true ->
          State = cache_remove(Key, EvKey, State0),
          Response = notfound
      end;
    [] ->
      State = State0,
      Response = notfound
  end,
  {Response, State}.

cache_insert(Key, Value, Expire, State) ->
  #state{ table = Table, eviction = Eviction0 } = State,
  {Eviction, EvKey} = kache_eviction:add(Eviction0, Key),
  true = ets:insert_new(Table, {Key, Value, Expire, EvKey}),
  State#state{ eviction = Eviction }.

cache_touch(Key, EvKey0, State) ->
  #state{ table = Table, eviction = Eviction0 } = State,
  {Eviction, EvKey} = kache_eviction:touch(Eviction0, EvKey0),
  true = ets:update_element(Table, Key, {4, EvKey}),
  State#state{ eviction = Eviction }.

cache_remove(Key, EvKey, State) ->
  #state{ table = Table, eviction = Eviction0 } = State,
  Eviction = kache_eviction:remove(Eviction0, EvKey),
  true = ets:delete(Table, Key),
  State#state{ eviction = Eviction }.

cache_purge(State) ->
  #state{ table = Table, eviction = Eviction0 } = State,
  Eviction = kache_eviction:purge(Eviction0),
  true = ets:delete_all_objects(Table),
  State#state{ eviction = Eviction }.

cache_wait(Key, From, State) ->
  ets:insert(State#state.waiting, {Key, From}),
  State.

cache_notify(Key, Response, State) ->
  #state{ waiting = Waiting } = State,
  Queue = ets:lookup(Waiting, Key),
  lists:foreach(fun({_, From}) -> gen_server:reply(From, Response) end, Queue),
  ets:delete(Waiting, Key),
  State.

-spec to_expire(ttl()) -> integer() | infinity.
to_expire(infinity) ->
  infinity;
to_expire({Unit, Ttl}) ->
  erlang:monotonic_time() + erlang:convert_time_unit(Ttl, Unit, native).

-spec is_expired(integer() | infinity) -> boolean().
is_expired(infinity) ->
  false;
is_expired(Expire) ->
  erlang:monotonic_time() > Expire.

