-module(kache_eviction).

-type impl() :: term().

-type strategy() :: {module(), impl()}.

-type key() :: term().

-type id() :: term().

-callback new() -> impl().

-callback delete(impl()) -> ok.

-callback add(impl(), key) -> {impl(), id()}.

-callback remove(impl(), id()) -> impl().

-callback touch(impl(), id()) -> {impl(), id()}.

-callback evict(impl(), non_neg_integer()) -> [{id(), key()}].

-callback purge(impl()) -> impl().

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

-spec new(module()) -> strategy().
new(Module) ->
  {Module, Module:new()}.

-spec delete(strategy()) -> ok.
delete({Module, Impl}) ->
  Module:delete(Impl).

-spec add(strategy(), key()) -> {strategy(), id()}.
add({Module, Impl0}, Key) ->
  {Impl, Id} = Module:add(Impl0, Key),
  {{Module, Impl}, Id}.

-spec remove(strategy(), id()) -> strategy().
remove({Module, Impl0}, Id) ->
  Impl = Module:remove(Impl0, Id),
  {Module, Impl}.

-spec touch(strategy(), id()) -> {strategy(), id()}.
touch({Module, Impl0}, Id0) ->
  {Impl, Id} = Module:touch(Impl0, Id0),
  {{Module, Impl}, Id}.

-spec evict(strategy(), non_neg_integer()) -> [{id(), key()}].
evict({Module, Impl}, N) ->
  Module:evict(Impl, N).

-spec purge(strategy()) -> strategy().
purge({Module, Impl0}) ->
  Impl = Module:purge(Impl0),
  {Module, Impl}.
