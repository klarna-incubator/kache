-module(kache_eviction_tests).

-include_lib("eunit/include/eunit.hrl").
-define(T(Function), {??Function, Function}).

kache_test_() ->
  [ ?T(fun() -> test_basics(kache_ev_none) end)
  , ?T(fun() -> test_basics(kache_ev_fifo) end)
  , ?T(fun() -> test_basics(kache_ev_lru) end)
  , ?T(fun() -> test_basics(kache_ev_random) end)
  , ?T(fun test_none/0)
  , ?T(fun test_fifo/0)
  , ?T(fun test_lru/0)
  , ?T(fun test_random/0)
  ].

test_basics(Module) ->
  run_seq(
    Module,
    [ {evict, 1, fun(L) -> ?assertEqual([], L) end}
    , {add, key1}
    , {add, key2}
    , {touch, key1}
    , {remove, key2}
    , purge
    , {evict, 1, fun(L) -> ?assertEqual([], L) end}
    ]).

test_none() ->
  run_seq(
    kache_ev_none,
    [ {add, key1}
    , {add, key2}
    , {evict, 2, fun(L) -> ?assertEqual([], L) end}
    ]).

test_fifo() ->
  run_seq(
    kache_ev_fifo,
    [ {add, key1}
    , {add, key2}
    , {add, key3}
    , {add, key4}
    , {evict, 1, fun(L) -> ?assertEqual([key1], L) end}
    , {touch, key2}
    , {evict, 1, fun(L) -> ?assertEqual([key2], L) end}
    , {remove, key3}
    , {evict, 1, fun(L) -> ?assertEqual([key4], L) end}
    ]).

test_lru() ->
  run_seq(
    kache_ev_lru,
    [ {add, key1}
    , {add, key2}
    , {add, key3}
    , {add, key4}
    , {evict, 1, fun(L) -> ?assertEqual([key1], L) end}
    , {touch, key2}
    , {evict, 1, fun(L) -> ?assertEqual([key3], L) end}
    , {remove, key4}
    , {evict, 1, fun(L) -> ?assertEqual([key2], L) end}
    ]).

test_random() ->
  run_seq(
    kache_ev_random,
    [ {add, key1}
    , {add, key2}
    , {add, key3}
    , {add, key4}
    , {evict, 1, fun(L) -> ?assertMatch([_], L) end}
    , {evict, 2, fun(L) -> ?assertMatch([_, _], L) end}
    , fun({_, Ids}=State) -> ?assertEqual(1, maps:size(Ids)), State end
    ]).

run_seq(Module, Operations) ->
  Init = kache_eviction:new(Module),
  {Final, _} = lists:foldl(fun run_op/2, {Init, #{}}, Operations),
  kache_eviction:delete(Final).

run_op({add, Key}, {Ev0, Ids}) ->
  {Ev, Id} = kache_eviction:add(Ev0, Key),
  {Ev, Ids#{ Key => Id }};
run_op({remove, Key}, {Ev0, Ids}) ->
  Ev = kache_eviction:remove(Ev0, maps:get(Key, Ids)),
  {Ev, maps:without([Key], Ids)};
run_op({touch, Key}, {Ev0, Ids}) ->
  {Ev, Id} = kache_eviction:touch(Ev0, maps:get(Key, Ids)),
  {Ev, Ids#{ Key := Id }};
run_op(purge, {Ev0, _}) ->
  Ev = kache_eviction:purge(Ev0),
  {Ev, #{}};
run_op({evict, N, Fun}, {Ev0, Ids0}) ->
  Results = kache_eviction:evict(Ev0, N),
  {EvKeys, Keys} = lists:unzip(Results),
  Fun(Keys),
  Ev = lists:foldl(fun(K, E) -> kache_eviction:remove(E, K) end, Ev0, EvKeys),
  Ids = maps:without(Keys, Ids0),
  {Ev, Ids};
run_op(Function, State) ->
  Function(State).

