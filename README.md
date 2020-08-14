# Kache - Smoooth Caching

[![Build Status][ci-image]][ci-url]
[![License][license-image]][license-url]
[![Developed at Klarna][klarna-image]][klarna-url]

A generic, in-memory caching application based on ETS.

```erlang
Cache = kache:start_link([compressed]),
kache:put(Cache, item1, value1),
{ok, value1} = kache:get(Cache, item1),
kache:remove(Cache, item1),
notfound = kache:get(Cache, item1),
kache:stop(Cache).
```

## Release History

See our [changelog](CHANGELOG.md).

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

### Cache Capacity and Eviction Strategies

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

## Contributing

See our guide on [contributing](.github/CONTRIBUTING.md).

### Development Setup

Kache is using `rebar3`.

```
$ rebar3 compile
$ rebar3 shell
```

The accompanying `Makefile` has a target to run unit tests and some
code analysis with `elvis`, `xref`, and `dialyzer`.

```
$ make ci
```

## License

Copyright © 2020 Klarna Bank AB

For license details, see the [LICENSE](LICENSE) file in the root of this project.

<!-- Markdown link & img dfn's -->
[ci-image]: https://github.com/klarna-incubator/kache/workflows/CI%20Pipeline/badge.svg
[ci-url]: https://github.com/klarna-incubator/kache/actions?query=workflow%3A%22CI+Pipeline%22
[license-image]: https://img.shields.io/badge/license-Apache%202-blue?style=flat-square
[license-url]: http://www.apache.org/licenses/LICENSE-2.0
[klarna-image]: https://img.shields.io/badge/%20-Developed%20at%20Klarna-black?labelColor=ffb3c7&style=flat-square&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAOCAYAAAAmL5yKAAAAAXNSR0IArs4c6QAAAIRlWElmTU0AKgAAAAgABQESAAMAAAABAAEAAAEaAAUAAAABAAAASgEbAAUAAAABAAAAUgEoAAMAAAABAAIAAIdpAAQAAAABAAAAWgAAAAAAAALQAAAAAQAAAtAAAAABAAOgAQADAAAAAQABAACgAgAEAAAAAQAAABCgAwAEAAAAAQAAAA4AAAAA0LMKiwAAAAlwSFlzAABuugAAbroB1t6xFwAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAVBJREFUKBVtkz0vREEUhsdXgo5qJXohkUgQ0fgFNFpR2V5ClP6CQu9PiB6lEL1I7B9A4/treZ47c252s97k2ffMmZkz5869m1JKL/AFbzAHaiRbmsIf4BdaMAZqMFsOXNxXkroKbxCPV5l8yHOJLVipn9/vEreLa7FguSN3S2ynA/ATeQuI8tTY6OOY34DQaQnq9mPCDtxoBwuRxPfAvPMWnARlB12KAi6eLTPruOOP4gcl33O6+Sjgc83DJkRH+h2MgorLzaPy68W48BG2S+xYnmAa1L+nOxEduMH3fgjGFvZeVkANZau68B6CrgJxWosFFpF7iG+h5wKZqwt42qIJtARu/ix+gqsosEq8D35o6R3c7OL4lAnTDljEe9B3Qa2BYzmHemDCt6Diwo6JY7E+A82OnN9HuoBruAQvUQ1nSxP4GVzBDRyBfygf6RW2/gD3NmEv+K/DZgAAAABJRU5ErkJggg==
[klarna-url]: https://github.com/klarna-incubator
