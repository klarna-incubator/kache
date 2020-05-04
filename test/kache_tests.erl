-module(kache_tests).

-include_lib("eunit/include/eunit.hrl").
-define(T(Function), {??Function, Function}).

kache_test_() ->
  [ ?T(fun test_put_get_remove/0)
  , ?T(fun test_independent_keys/0)
  , ?T(fun test_independent_caches/0)
  , ?T(fun test_ttl/0)
  , ?T(fun test_purge/0)
  , ?T(fun test_info/0)
  , ?T(fun test_named/0)
  ].

test_put_get_remove() ->
  {ok, Cache} = kache:start_link([]),
  ?assertEqual(notfound, kache:get(Cache, key)),
  kache:put(Cache, key, value),
  ?assertEqual({ok, value}, kache:get(Cache, key)),
  kache:remove(Cache, key),
  ?assertEqual(notfound, kache:get(Cache, key)),
  kache:stop(Cache).

test_independent_keys() ->
  {ok, Cache} = kache:start_link([]),
  kache:put(Cache, key1, value1),
  kache:put(Cache, key2, value2),
  ?assertEqual({ok, value1}, kache:get(Cache, key1)),
  ?assertEqual({ok, value2}, kache:get(Cache, key2)),
  kache:stop(Cache).

test_independent_caches() ->
  {ok, Cache1} = kache:start_link([]),
  {ok, Cache2} = kache:start_link([]),
  kache:put(Cache1, key, value1),
  kache:put(Cache2, key, value2),
  ?assertEqual({ok, value1}, kache:get(Cache1, key)),
  ?assertEqual({ok, value2}, kache:get(Cache2, key)),
  kache:stop(Cache1),
  kache:stop(Cache2).

test_ttl()->
  {ok, Cache} = kache:start_link([]),
  kache:put(Cache, key, value, {millisecond, 10}),
  ?assertEqual({ok, value}, kache:get(Cache, key)),
  timer:sleep(10),
  ?assertEqual(notfound, kache:get(Cache, key)),
  kache:stop(Cache).

test_purge()->
  {ok, Cache} = kache:start_link([]),
  kache:put(Cache, key1, value),
  kache:put(Cache, key2, value),
  kache:purge(Cache),
  ?assertEqual(notfound, kache:get(Cache, key1)),
  ?assertEqual(notfound, kache:get(Cache, key2)),
  kache:stop(Cache).

test_info()->
  {ok, Cache} = kache:start_link([]),
  Info0 = kache:info(Cache),
  ?assertEqual(0, proplists:get_value(size, Info0)),
  kache:put(Cache, key, value),
  Info1 = kache:info(Cache),
  ?assertEqual(1, proplists:get_value(size, Info1)),
  kache:purge(Cache),
  Info2 = kache:info(Cache),
  ?assertEqual(0, proplists:get_value(size, Info2)),
  ?assert(proplists:get_value(memory, Info0) < proplists:get_value(memory, Info1)),
  kache:stop(Cache).

test_named() ->
  Cache = cache,
  {ok, _} = kache:start_link({local, Cache}, []),
  ?assertEqual(notfound, kache:get(Cache, key)),
  kache:put(Cache, key, value),
  ?assertEqual({ok, value}, kache:get(Cache, key)),
  kache:remove(Cache, key),
  ?assertEqual(notfound, kache:get(Cache, key)),
  kache:stop(Cache).
