-module(kache_ev_fifo).
-behaviour(kache_eviction).

-record(impl,
        { queue :: ets:tab()
        , next  :: integer()
        }).

-type impl() :: #impl{}.

-type id() :: integer().

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
  Table = ets:new(?MODULE, [ordered_set]),
  #impl{ queue = Table, next = 0 }.

-spec delete(impl()) -> ok.
delete(Impl) ->
  ets:delete(Impl#impl.queue),
  ok.

-spec add(impl(), term()) -> {impl(), id()}.
add(Impl, Key) ->
  #impl{ queue = Queue, next = Id } = Impl,
  ets:insert(Queue, {Id, Key}),
  {Impl#impl{ next = Id + 1 }, Id}.

-spec remove(impl(), id()) -> impl().
remove(Impl, Id) ->
  ets:delete(Impl#impl.queue, Id),
  Impl.

-spec touch(impl(), id()) -> {impl(), id()}.
touch(Impl, Id) ->
  {Impl, Id}.

-spec evict(impl(), non_neg_integer()) -> [{id(), term()}].
evict(_, 0) ->
  [];
evict(Impl, N) ->
  case ets:match_object(Impl#impl.queue, '_', N) of
    {Results, _} ->
      Results;
    '$end_of_table' ->
      []
  end.

-spec purge(impl()) -> impl().
purge(Impl) ->
  ets:delete_all_objects(Impl#impl.queue),
  Impl#impl{ next = 0 }.
