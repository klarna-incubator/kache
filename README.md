# Kache - Smoooth Caching

A generic, in-memory caching application based on ETS.

```erlang
Cache = kache:start_link([compressed]),
kache:put(Cache, item1, value1),
{ok, value1} = kache:get(Cache, item1),
kache:remove(Cache, item1),
notfound = kache:get(Cache, item1),
kache:stop(Cache).
```

## Features

* Simple key/value interface
* Automatic, per-item time-to-live
* Optional capacity limit with multiple eviction strategies
* Synchronized cache get-or-fill API to avoid stampeding

## Usage

The public library interface is implemented in the [`kache`
module](src/kache.erl).

### Creation/Destruction

Use `start_link` to create a new cache.  The behavior of the
cache can be tuned through the options `proplist`.  To destroy the
cache, call `stop`.

```erlang
Cache = kache:start_link([]),
kache:stop(Cache).
```

### Key/value Interface

The simplest interface to a cache simply treats it as a key-value
store with the `put`, `get`, `remove`, and `purge` functions.

```erlang
kache:put(Cache, Key, Value, TimeToLive),
{ok, Value} = kache:get(Cache, Key),
kache:remove(Cache, Key),
kache:purge(Cache).
```

### Entry Expiration

Each entry in the cache has an associated time-to-live.  There are two
mechanisms to remove expired entries from the cache.

1) The time-to-live is evaluated on each `get` and stale entries
automatically removed before the function returns `notfound`.

2) The `sweep` function performs a full cache scan and removes all
stale items.

### Synchronized Interface

For a cache with heavy read-to-write ratio, the simple key/value
interface may be insufficient to properly protect the underlying
resource.  In this case, the `get_wait` and `get_fill` functions
provide a mechanism to limit the number of processes competing to put
a value into the cache.

The function `get_wait` returns immediately with `{ok, Value}` if a
value is already cached.  If not, it waits for a value to be `put`
into the cache, or the specified `Timeout` to expire.  In case of a
timeout or an explicit `remove`, it returns `notfound`.

```erlang
timer:apply_after(Delay, kache, put, [Cache, Key, Value]),
{ok, Value} = kache:get_wait(Cache, Key, Timeout).
```

The function `get_fill`, when no value is cached and no other process
is already running `get_fill`, calls the `Generator` argument to
produce a value and puts it into the cache.  Otherwise, it behaves as
`get_wait`.

```erlang
{ok, Value} = kache:get_fill(Cache, Key, Generator, Timeout).
```

## Cache Capacity and Eviction Strategies

An eviction strategy is any module that implements the
[`kache_eviction` behavior](src/kache_eviction.erl).  Four eviction
strategies are included

* `none` - does not evict anything and does not enforce the capacity limit
* `fifo` - evicts in order of insertion, oldest entries first
* `lru` - evicts in order of use, longest unread entries first
* `random` - evicts in randomized order

The function `evict` asks the eviction strategy to remove at least `N`
entries from the cache.  Depending on the eviction strategy, any
number of entries might actually be removed, including none and all..

```erlang
Cache = kache:start_link([{eviction, lru}, {capacity, 1024}]),
kache:evict(Cache, N),
```
