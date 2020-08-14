-module(kache_ev_none).
-behaviour(kache_eviction).

-type impl() :: {}.

-type id() :: {}.

-export([ new/0
        , delete/1
        , add/2
        , remove/2
        , touch/2
        , evict/2
        , purge/1
        ]).

-spec new() -> impl().
new() ->
  {}.

-spec delete(impl()) -> ok.
delete({}) ->
  ok.

-spec add(impl(), term()) -> {impl(), id()}.
add({}, _) ->
  {{}, {}}.

-spec remove(impl(), id()) -> impl().
remove({}, {}) ->
  {}.

-spec touch(impl(), id()) -> {impl(), id()}.
touch({}, {}) ->
  {{}, {}}.

-spec evict(impl(), non_neg_integer()) -> [{id(), term()}].
evict({}, _) ->
  [].

-spec purge(impl()) -> impl().
purge({}) ->
  {}.
