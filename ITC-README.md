# lucuma-itc


This is a graphql server acting as a proxy for the old ocs2-based itc server

# Run

It is possible to run locally using sbt

```
   sbt ~service/reStart
```

When using sbtn there is no output (see https://github.com/spray/sbt-revolver/issues/99).

Note, it is important to have [`git lfs`](https://git-lfs.com) installed in
order to obtain the necessary classes for running the ITC.  See (`git lfs` and
Legacy ITC Code below).

You can then open the playground to work with the API by pointing your browser to

http://localhost:6060/playground.html


## Env

The app needs an environment variable, `REDISCLOUD_URL`, which points to the
redis server used for caching. For example: `REDISCLOUD_URL = "redis://localhost"`.

## Caching

ITC calculations are relatively expensive and they are pure (a given input always produces the same output)
the lucuma ITC server uses redis to store the results linking them from the request parameters to the results.

The cache design is optimized to use minimal space given some of the responses (graphs) are fairly large.
We are also assuming we'll never need to go inside the cached data to edit the data and we can
discard items at any moment.

The keys are stored as `itc:prefix:hash` where hash is just the hash of the parameters

A few diferent encodings were tested to reduce size. Here are some measurement

* Plain json: 1441864
* Compressed json: 589896
* Boopickle: 262216

Make sure Redis is configured to use the `volatile-lru` eviction policy. This allows us to use it as a cache
for keys stored with a TTL, which is what we do here, while keeping a permanent entry for the version key.
If the version key is missing or changes, the cache is flushed.

In Heroku's "Redis Cloud" add-on, `volatile-lru` is the default at the time of writing of this, but it should
be verified. This can be configured under `Configuration`/`Durability`/`Data eviction policy`:

![Heroku Redis Cloud volatility configuration](heroku-redis-volatility.png)

## Cache flushing

The only reason for the remote values to be stale is if the old ITC changes (happens not very often).
`lucuma-itc` will check on startup and verify if the ITC version has changed. If so it will flush the whole cache.

## `git lfs` and Legacy ITC Code

The itc calculations are mostly done in java and scala using legacy technologies,
in particular libraries like scala 2.11, scalaz, argonaut.  We were wrapping this
code in an http server but that incurred considerable overhead especially when
the graph data was needed.  As an alternative we can now directly call the java
code but given the use of legacy libraries this requires the jar files to be
loaded dynamically by the application and be called via reflection with a custom
classloader

In case the code in ocs2 changes we need to update the jar files using the
`update.sh` script.  The jar files are fairly large (they contain the data files
used to calculate the itc results). Given github limitations these need to be
stored in `git lfs`.

[Install `git lfs`](https://git-lfs.com) to get the necessary jar files.

## Long term

Ideally we'd port the old ITC codebase and integrate it here. This is no small task but an initial
attempt was started on the `legacy-port` branch

## Buildpacks

This project needs the following buildpacks to be used in heroku (order is important)

```
=== itc-master Buildpack URLs
1. https://github.com/radian-software/heroku-buildpack-git-lfs
2. heroku/scala
3. https://github.com/opencounter/heroku-buildpack-post-build-clean.git
```

First we need git lfs to get the files stored in github lfs
Then scala to build the app
And finally post-build-clean to reduce the slug size

## Schema stitching

The itc schema uses many types defined on the odb schema. The `schemastitcher.js` script can be used
to generate a single schema file that includes all the types needed by the itc schema. It uses as a
base `itc_base.graphql` and stitches it with `ObservationDB.graphql`.

It starts with the queries defined on `itc_base.graphql` and prunes the unused types, as itc doesn't
need most of `ObservationDB.graphql` types.

Call it as:
```
node itc/schemastitcher.js modules/service/src/main/resources/graphql/ObservationDB.graphql itc/service/src/main/resources/graphql/itc_base.graphql itc/service/src/main/resources/graphql/itc.graphql
```

The script `itcFetchODBSchema.sh` will conveniently perform this invocation.

## Benchmarking

### Simple Performance Test
```
sbt "benchmark/runMain lucuma.itc.benchmarks.ItcPerformanceHarness"
```

### Using jmh
```
benchmark/Jmh/run lucuma.itc.benchmarks.ItcCoreBenchmark
```

