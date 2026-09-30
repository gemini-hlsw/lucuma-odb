# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

This file covers the `resource/` module. The repository root has no CLAUDE.md.

## What the service is

The Resource service is a telescope calendar and a resource manager. It replaces ICTD. It stores facts as interval blocks: telescope availability, telescope mode, ToO support, instrument availability, subsystem availability, and instrument component availability. Each fact is `[start, end)`, start inclusive and end exclusive. Two blocks for the same subject never overlap. The service is read-only in v1. Mutations and subscriptions are stubs and are not wired in.

## Commands

Run all commands from the repository root. The sbt projects are `resourceModel` and `resourceService`.

```
sbt resourceService/compile
sbt resourceService/test
sbt "resourceService/testOnly lucuma.resource.graphql.query.TooSupportSuite"
sbt resourceService/clean
sbt '~resourceService/reStart'
```

Static checks that CI runs on the module:

```
sbt headerCheckAll scalafmtCheckAll lucumaScalafixCheck "scalafixAll --check"
sbt headerCreateAll scalafmtAll scalafixAll
```

Tests need Docker. The test suite builds a Postgres image from `resource/service/src/Dockerfile` with all migrations applied. CI prebuilds the image and sets `RESOURCE_TEST_DB_IMAGE`. Locally, leave that variable unset.

To run the server locally, export the variables in `resource/.env` first, for example with `set -a; source resource/.env; set +a`. No sbt plugin reads the file. It sets `DATABASE_URL`, the SSO variables, and `RESOURCE_DOMAIN`. The server listens on port 8484. The playground is at `http://localhost:8484/resource/playground.html`. Set `RESET_DATABASE=true` to drop and recreate the database at startup. Set `SKIP_MIGRATION=true` to skip Flyway.

## Rules that are easy to break

**Editing the GraphQL schema needs a clean build.** `GraphQlRoutes.loadSchema` calls `SchemaStitcher.load`, which is an inline macro. The macro bakes the stitched schema text into the class at compile time. The incremental compiler does not track the `.graphql` file. After you edit `resource.graphql`, run `sbt resourceService/clean` before you compile or test. Without the clean, the server runs the old schema and reports errors such as "Unknown field(s)".

**Never copy ODB or SSO code into `resource/`.** If a helper exists in `modules/service`, `modules/sso-service`, or `modules/binding`, move it to `modules/schema` (cross JVM and JS, no grackle) or `modules/common-middleware` (JVM, grackle-core). Keep the original package name when you move it. `resourceService` must never depend on the ODB `service` project.

**Each enum has three spellings.** The Scala case (`TooSupport.Standard`), the Scala tag and Postgres enum label (`'Standard'`), and the GraphQL value (`STANDARD`). The migration comment says: enum members spell the Scala `Enumerated` tags. `Codecs.enumerated(Type("e_..."))` maps by tag. Test seeds insert the tag, and test queries use the GraphQL value.

**Blocks have no id.** Every query can clip a block to the requested window, so no id stays stable. The mapping keeps `_id`, `_start`, and `_end` as hidden fields for ordering and clipping only.

## Architecture

Three layers, all in `resource/service/src/main/scala/resource/server/`:

- `http4s/`: `ResourceMain` loads config with ciris, runs Flyway, opens a skunk pool, and mounts routes. `/` serves static files and `/resource` serves GraphQL over HTTP and WebSocket. `ServerMiddleware` adds CORS, GZip, and OpenTelemetry. Authentication comes from `lucuma.common.middleware.UserContext` and the SSO client.
- `graphql/`: `ResourceMapping` is a grackle `SkunkMapping`. It mixes in one trait per concern and combines them in `typeMappings` and `selectElaborator`. `BaseMapping` holds every `schema.ref(...)`, the `MaxWindow` limit of 400 days, and `validateWindow`. `graphql/table/` holds the `TableDef` objects. `BlockTable` is the shared column set of every block table.
- Root package: `Codecs` (skunk codecs, extends the shared `CoreCodecs`), `NightProjection`, and `ComponentCatalog`.

`resource/model/` holds the `Enumerated` enums and the ciris config case classes. It depends on `modules/schema`, `modules/otel`, and the SSO client. Keep it free of grackle and skunk.

### Two query paths

The flat block queries (`tooSupport`, `telescopeAvailability`, `telescopeMode`, `instrumentAvailability`, `telescopeSubsystemAvailability`, `instrumentComponentAvailability`, `components`, `publishedSemesters`) go through grackle SQL mappings. `QueryMapping.blockQuery` builds one elaborator per block type. It validates the window, filters by overlap with `c_end > start and c_start < end`, orders by start, and puts the window in the `Env` when `clip` is true. `TimestampIntervalMapping.blockMapping` gives every block type its shared fields and derives `interval` from the hidden `_start` and `_end` columns.

The night queries (`telescopeNight`, `telescopeNights`) are grackle `RootEffect` handlers backed by `NightProjection`. Grackle cannot join by interval overlap, so `NightProjection` runs its own skunk SQL and builds JSON. It reads only the block tables the client selected. It must produce the same JSON shape as the SQL mappings for the same types, so it uses the same circe encoders.

### Database

One migration file, `V0000__resource_blocks.sql`, holds the v1 schema. Every block table has a generated `c_interval tsrange` column, a GiST exclusion constraint against overlap, and two btree indexes on `(c_site, c_start)` and `(c_site, c_end)`. Text columns use domains such as `d_nonempty_text` so that a bad value fails at write time, not at every later read. `v_instrument_component_at_site` serves the `components` root.

Flyway runs at server startup. Tests never run Flyway: the migrations are applied when the Docker image is built.

### Schema stitching

`resource.graphql` starts with an `#import` line that pulls scalars and shared types from the ODB schema in `modules/schema`. `SchemaStitcher` in `modules/binding` joins the two at compile time. CI validates schema changes with graphql-inspector against `main`.

## Tests

Test packages live under `lucuma.resource`. Extend `ResourceGraphQLSuite` for a GraphQL test. It gives you:

- `seed`: override it to insert rows with `exec(sql"...".command)`. It runs once per suite, after the container starts.
- `expectSuccess(query, expected)`: compares pretty-printed JSON, so use the `json"..."` literal.
- `expect(query, Left(List("message")))`: asserts GraphQL error messages.
- `expectFailure(query)`: asserts HTTP 403 for the anonymous user.
- `asUser(user)`, `anonymous`, and `rawAuthorization(...)` for the `authorization` argument. `TestSso` signs JWTs with a generated key, so no SSO server is needed.

Each suite gets one Postgres container and one embedded server. Seeds from different suites do not share a container. `ClientOption.Ws` runs the same query over WebSocket.
