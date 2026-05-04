# ITC Contributor Guide (CLAUDE)

This document describes the ITC area under `itc/` in this repository, with a focus on active sbt projects, common commands, and operational gotchas.

## Scope

Everything in this file applies to the ITC subtree rooted at `itc/`.

## Active ITC sbt Projects

Project IDs are taken from `build.sbt` and should be used in sbt commands.

| sbt project id   | Path               | Role                                                                |
| ---------------- | ------------------ | ------------------------------------------------------------------- |
| `itcModel`       | `itc/model`        | Shared ITC domain/model types (JVM + JS cross project)              |
| `itcClient`      | `itc/client`       | ITC GraphQL client and input/result codecs (JVM + JS cross project) |
| `itcService`     | `itc/service`      | ITC GraphQL server and legacy ITC bridge                            |
| `itcTestkit`     | `itc/testkit`      | Arbitraries/laws support for testing                                |
| `itcTests`       | `itc/tests`        | Main ITC test suites                                                |
| `itcLegacyTests` | `itc/legacy-tests` | Regression checks against legacy behavior (opt-in)                  |
| `itcBenchmark`   | `itc/benchmarks`   | Performance harness + JMH benchmarks                                |

## Dependency Shape (High-Level)

- `itcService` depends on `itcModel.jvm` and shared repo modules (`binding`, `otel`).
- `itcClient` depends on `itcModel`.
- `itcTestkit` depends on `itcClient`.
- `itcTests` and `itcLegacyTests` depend on `itcService`, `itcClient.jvm`, and `itcTestkit.jvm`.
- `itcBenchmark` depends on `itcService`.

## Important Directories

- `itc/service/src/main/scala/lucuma/itc/service/Main.scala`: service entrypoint and middleware wiring.
- `itc/service/ocslib/`: legacy OCS JARs loaded via custom classloader.
- `itc/service/src/main/resources/graphql/`: ITC schema files (`itc_base.graphql`, generated `itc.graphql`).
- `itc/client/shared/src/main/scala/lucuma/itc/client/`: shared client API and GraphQL query fragments.
- `itc/tests/src/test/scala/`: primary functional and GraphQL tests.
- `itc/legacy-tests/src/test/scala/`: legacy parity tests.

## Common Commands

Run from repository root.

### Start ITC service (hot-reload)

```bash
sbt ~itcService/reStart
```

### Run standard ITC tests

```bash
sbt itcTests/test
```

### Run legacy ITC tests (disabled by default)

```bash
RUN_LEGACY_TESTS=true sbt itcLegacyTests/test
```

### Run benchmarks

Simple harness:

```bash
sbt "itcBenchmark/runMain lucuma.itc.benchmarks.ItcSpeedTest"
```

JMH benchmark class:

```bash
sbt "itcBenchmark/Jmh/run lucuma.itc.benchmarks.ItcCoreBenchmark"
```

## Schema Stitching Workflow

Scripts:

- `itc/stitchSchema.sh`
- `itc/fetchODBSchema.mjs`
- `itc/schemastitcher.mjs`

Generate stitched ITC schema:

```bash
./itc/stitchSchema.sh local
```

Supported environments: `local`, `dev`, `staging`, `production`.

Output schema path:

- `itc/service/src/main/resources/graphql/itc.graphql`

Notes:

- `stitchSchema.sh` runs `npm ci` and requires Node/npm.
- `local` mode expects ODB at `http://localhost:8082/odb`.

## Legacy OCS JAR Refresh

Use when upstream OCS ITC bundle changes.

```bash
./itc/update_itc_jars.sh <path-to-ocs-itc-bundle>
```

What it does:

- Copies required legacy JARs into `itc/service/ocslib/`.
- Captures git metadata from the OCS repository.
- Writes `itc/service/ocslib/build-info.json`.

Use `-f` only when you intentionally want to skip OCS git cleanliness checks.

## Environment Variables and Runtime Notes

- `REDISCLOUD_URL`: enables Redis-backed cache. If absent, service uses no-op cache.
- `RUN_LEGACY_TESTS=true`: required to execute `itcLegacyTests`.
- `ODB_BASE_URL`: defaults in build settings for local development; override as needed.

Service defaults:

- ITC service Docker exposes port `6060`.

## Working Conventions

- Prefer edits in active modules listed above; ignore `target/` artifacts.
- Keep GraphQL schema changes paired with stitched schema regeneration when needed.
- When changing legacy interop behavior, run both `itcTests` and `itcLegacyTests`.
- Avoid committing generated/transient files unless intentionally updating derived artifacts.

## Historical/Orphaned Area

- `itc/itc/src/test/...` contains an old test (`ItcChartSuite`) not wired as an sbt project in current `build.sbt`.
- Treat it as historical unless you explicitly add/restore project wiring.
