%% @doc Behavior and entrypoint for cache eviction strategies.
-module(kache_eviction).

-type impl() :: term().
%% Opaque type for the eviction strategy implementation.

-type strategy() :: {module(), impl()}.
%% Eviction strategy object.

-type key() :: term().
%% Key used in the cache.

-type id() :: term().
%% Id assigned to a key by the eviction strategy.

-callback new() -> impl().

-callback delete(impl()) -> ok.

-callback add(impl(), key) -> {impl(), id()}.
%% Called when an entry is inserted into the cache.
%%
%% May assume that `Key' has not been added before or has been
%% removed.

-callback remove(impl(), id()) -> impl().
%% Called when an entry is removed from the cache.
%%
%% May assume that `Key' has been added before and has not been
%% removed.

-callback touch(impl(), id()) -> {impl(), id()}.
%% Called when an entry is read from the cache.
%%
%% May assume that `Key' has been added before and has not been
%% removed.

-callback evict(impl(), non_neg_integer()) -> [{id(), key()}].
%% Called to produce a list of keys to evict.
%%
%% Must not modify the strategy.

-callback purge(impl()) -> impl().
%% Called when the cache is purged.
%%
%% Assumed to be more efficient than calling `remove' for every key.

-export_type([ strategy/0
             , key/0
             , id/0
             ]).

-export([ new/1
        , delete/1
        , add/2
        , touch/2
        , remove/2
        , evict/2
        , purge/1
        ]).

%% @doc Create a new eviction strategy for the give module.
-spec new(module()) -> strategy().
new(Module) ->
  {Module, Module:new()}.

%% @doc Delete an eviction strategy.
-spec delete(strategy()) -> ok.
delete({Module, Impl}) ->
  Module:delete(Impl).

%% @doc Add a key to the eviction strategy.  Must not be called
%% multiple times with the same `Key' without calling `remove' in
%% between.
-spec add(strategy(), key()) -> {strategy(), id()}.
add({Module, Impl0}, Key) ->
  {Impl, Id} = Module:add(Impl0, Key),
  {{Module, Impl}, Id}.

%% @doc Remove a key from the eviction strategy.  Must not be called
%% without a previous `add'.
-spec remove(strategy(), id()) -> strategy().
remove({Module, Impl0}, Id) ->
  Impl = Module:remove(Impl0, Id),
  {Module, Impl}.

%% @doc Notify the eviction strategy that a key has been used.  The
%% key must have been `add'ed before and not been `remove'd.
-spec touch(strategy(), id()) -> {strategy(), id()}.
touch({Module, Impl0}, Id0) ->
  {Impl, Id} = Module:touch(Impl0, Id0),
  {{Module, Impl}, Id}.

%% @doc Produce a list of keys to be evicted.  `N' is the suggested
%% number of keys to return.
-spec evict(strategy(), non_neg_integer()) -> [{id(), key()}].
evict({Module, Impl}, N) ->
  Module:evict(Impl, N).

%% @doc Remove all entries from the eviction strategy.
-spec purge(strategy()) -> strategy().
purge({Module, Impl0}) ->
  Impl = Module:purge(Impl0),
  {Module, Impl}.
