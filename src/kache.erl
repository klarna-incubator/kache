-module(kache).
-behaviour(gen_server).

-define(DEFAULT_TIMEOUT, 5000).

-type ttl() :: {erlang:time_unit(), integer()} | infinity.

-type option() :: compressed.

-type cache() :: pid() | atom().

-export_type([ ttl/0
             , option/0
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
        , remove/2
        , remove/3
        , purge/1
        , purge/2
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-spec start_link([option()]) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
  gen_server:start_link(?MODULE, Options, []).

-spec start_link(ServerName, [option()]) -> {ok, pid()} | {error, term()}
          when ServerName :: {local, atom()} | {global, atom()}.
start_link(ServerName, Options) ->
  gen_server:start_link(ServerName, ?MODULE, Options, []).

-spec stop(cache()) -> ok.
stop(Cache) ->
  gen_server:stop(Cache).

-spec put(cache(), term(), term()) -> ok.
put(Cache, Key, Value) ->
  put(Cache, Key, Value, infinity).

-spec put(cache(), term(), term(), ttl()) -> ok.
put(Cache, Key, Value, Ttl) ->
  put(Cache, Key, Value, Ttl, ?DEFAULT_TIMEOUT).

-spec put(cache(), term(), term(), ttl(), timeout()) -> ok.
put(Cache, Key, Value, Ttl, Timeout) ->
  gen_server:call(Cache, {put, Key, Value, Ttl}, Timeout).

-spec get(cache(), term()) -> {ok, term()} | notfound.
get(Cache, Key) ->
  get(Cache, Key, ?DEFAULT_TIMEOUT).

-spec get(cache(), term(), timeout()) -> {ok, term()} | notfound.
get(Cache, Key, Timeout) ->
  gen_server:call(Cache, {get, Key}, Timeout).

-spec remove(cache(), term()) -> ok.
remove(Cache, Key) ->
  remove(Cache, Key, ?DEFAULT_TIMEOUT).

-spec remove(cache(), term(), timeout()) -> ok.
remove(Cache, Key, Timeout) ->
  gen_server:call(Cache, {remove, Key}, Timeout).

-spec purge(cache()) -> ok.
purge(Cache) ->
  purge(Cache, ?DEFAULT_TIMEOUT).

-spec purge(cache(), timeout()) -> ok.
purge(Cache, Timeout) ->
  gen_server:call(Cache, purge, Timeout).

%% gen_server

-record(state, { table :: ets:tid() }).

init(Options) ->
  EtsOptions = lists:flatmap(fun ets_option/1, Options),
  Table = ets:new(?MODULE, [set | EtsOptions]),
  State = #state{ table = Table },
  {ok, State}.

-spec ets_option(option()) -> [term()].
ets_option(compressed) ->
  [compressed].

handle_call({put, Key, Value, Ttl}, _, State) ->
  {Reply, NewState} = do_put(Key, Value, Ttl, State),
  {reply, Reply, NewState};
handle_call({get, Key}, _, State) ->
  {Reply, NewState} = do_get(Key, State),
  {reply, Reply, NewState};
handle_call({remove, Key}, _, State) ->
  {Reply, NewState} = do_remove(Key, State),
  {reply, Reply, NewState};
handle_call(purge, _, State) ->
  {Reply, NewState} = do_purge(State),
  {reply, Reply, NewState};
handle_call(_, _, State) ->
  {noreply, State}.

handle_cast(_, State) ->
  {noreply, State}.

do_put(Key, Value, Ttl, State) ->
  Expire = to_expire(Ttl),
  ets:insert(State#state.table, {Key, Value, Expire}),
  {ok, State}.

do_get(Key, State) ->
  case ets:lookup(State#state.table, Key) of
    [{Key, Value, Expire}] ->
      case is_expired(Expire) of
        false ->
          Response = {ok, Value};
        true ->
          Response = notfound
      end;
    [] ->
      Response = notfound
  end,
  {Response, State}.

do_remove(Key, State) ->
  ets:delete(State#state.table, Key),
  {ok, State}.

do_purge(State) ->
  ets:delete_all_objects(State#state.table),
  {ok, State}.

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

