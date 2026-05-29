# lucuma-odb Contributor Guide (CLAUDE)

This file covers the ODB subsystem. The ITC subsystem has its own guide at `itc/CLAUDE.md`.

## Repository Layout

| sbt project id | Path | Role |
|---|---|---|
| `schema` | `modules/schema` | Cross (JVM+JS) — GraphQL schema, JSON codecs, domain types |
| `otel` | `modules/otel` | OpenTelemetry/natchez wiring |
| `binding` | `modules/binding` | Grackle input binding helpers (JVM) |
| `sequence` | `modules/sequence` | Sequence model — observing mode logic, step generation |
| `smartgcal` | `modules/smartgcal` | SmartGCal calibration lookup |
| `phase0` | `modules/phase0` | Phase 0 / proposal types |
| `service` | `modules/service` | Main ODB service: DB, Grackle mappings, GraphQL resolvers |
| `obscalc` | `modules/obscalc` | Observation calculator service (depends on service) |
| `calibrations` | `modules/calibrations` | Calibrations service (depends on service) |

The `sso_*` projects are a separate auth subsystem in this repo; treat them as an independent concern.

## Common Commands

Run from repository root.

### Compile

```bash
sbt service/compile
sbt schemaJVM/compile
sbt sequence/compile
sbt service/Test/compile
```

### Test

```bash
sbt service/test
sbt "service/testOnly lucuma.odb.graphql.mutation.createObservation_GnirsLongSlit"
```

### Run service (hot-reload, port 8082)

```bash
sbt service/reStart
# or start all three services together:
sbt allStart
sbt allStop
```

`reStart / envVars` sets `PORT=8082`; the ODB endpoint is `http://localhost:8082/odb`.

### Static checks

```bash
sbt scalafixAll headerCreate
```

## Dependency Versions

A single `lucumaCoreVersion` variable covers **all** `edu.gemini` artifacts:
`lucuma-core`, `lucuma-core-testkit`, `lucuma-catalog`, `lucuma-catalog-testkit`,
`lucuma-horizons`, `lucuma-ags`. Do **not** introduce a separate version variable for these.

When testing against an unreleased lucuma-core change:
1. In the sibling `../lucuma-core` repo, run `sbt publishLocal` — this produces a SNAPSHOT like `0.198.0-1-<hash>-SNAPSHOT`.
2. Update `lucumaCoreVersion` in `build.sbt` to that SNAPSHOT string.

## DB Migrations (Flyway)

Migration files live in `modules/service/src/main/resources/db/migration/` and follow the naming convention `V{number}__{description}.sql`.

When adding a new observing mode, the migration must:

1. Create any lookup tables the mode needs (enums, etc.).
2. Create the main mode table `t_{mode}` with:
   - `c_observation_id` as `NOT NULL PRIMARY KEY`
   - `c_observing_mode_type e_observing_mode_type NOT NULL DEFAULT '{mode_tag}'`
   - a `CHECK` constraint asserting the type equals the literal tag
   - a `FOREIGN KEY (c_observation_id, c_observing_mode_type) REFERENCES t_observation(...) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED`
3. Create a view `v_{mode}` that selects from the table and adds computed `_default` columns.
4. Update `v_observing_mode_group` (a `UNION ALL` view) to include the new mode.
5. Call `SELECT register_observing_mode('{mode_tag}', 't_{mode}');`.
6. Update the `check_etm_consistent` PL/pgSQL function to handle the new mode tag.
7. If the new mode needs a `c_configuration_request` column (for configuration grouping), `ALTER TABLE t_configuration_request ADD COLUMN ...`, then `DROP VIEW v_configuration_request` and recreate it adding a new `CASE WHEN` column for the mode.

**PostgreSQL domain types to use for offsets:** `d_angle_µas` (not `d_offset_component`, which doesn't exist).

## Adding a New Observing Mode — Checklist

When adding an instrument mode (e.g., `gnirs_long_slit`), changes are needed in these locations:

### `modules/schema`
- `OdbSchema.graphql` — Add output type `FooLongSlit`, input types `FooLongSlitInput`, `FooLongSlitAcquisitionInput`, etc. Add the mode to `ObservingModeType` enum, `ObservingModeInput`, and `ObservingMode` union/type. **See GraphQL Schema Pitfalls below.**
- `src/main/scala/lucuma/odb/json/configurationrequest.scala` — Add a decoder and encoder case for the new `Configuration.ObservingMode` variant.

### `modules/binding`
- Add `Foo*Binding.scala` files for each new enum that needs a Grackle input binding.

### `modules/sequence`
- `ObservingMode.scala` — Add the new mode to the `ObservingMode` sealed trait and its companion.
- `gnirs/` (or `foo/`) — Sequence generation logic for the new mode.

### `modules/service`

**DB:**
- `src/main/resources/db/migration/V{N}__foo_long_slit.sql` — See DB Migrations section above.

**GraphQL input:**
- `graphql/input/FooLongSlitInput.scala` — `Create` and `Edit` case classes. `Edit` must implement `def toCreate: Result[Create]`.

**Service:**
- `service/FooLongSlitService.scala` — CRUD logic using Skunk. The INSERT codec contramap must emit values for **every** column in the table, including `c_initial_*` columns (each initial value is a separate column that must be duplicated in the contramap output).
- `service/ObservingModeServices.scala` — Add the new mode to the mode dispatcher.
- `service/Services.scala` — Wire the new service into the DI trait + implementation.
- `service/ConfigurationService.scala` — Add a `case` in `selectMany` for the new `ObservingModeType` / `Configuration.ObservingMode` variant.
- `service/CalibrationConfigSubset.scala` — All call sites for `ObservingModeInput.Create(...)` must be updated when a new field is added to the constructor (add `none` for the new parameter).

**Grackle mappings:**
- `graphql/mapping/FooLongSlitMapping.scala` — `ObjectMapping` for the output type + `SqlField` / `CursorField` bindings.
- `graphql/mapping/ObservingModeMapping.scala` — Add the new mode type to the mapping.
- `graphql/BaseMapping.scala` — Register the new mapping.
- `graphql/OdbMapping.scala` — Add the new mapping to the type map.
- `graphql/table/FooLongSlitView.scala` — Table/view column definitions.

**Tests:**
- `src/test/scala/lucuma/odb/graphql/mutation/createObservation_FooLongSlit.scala`
- `src/test/scala/lucuma/odb/graphql/DatabaseOperations.scala` — Add `createFooLongSlitObservationAs` helper; implement any `???` stubs in `observingModeObject` / `observingModeWithSpatialOffsets` for the new mode.

**StartupDiagnostics:**
- `StartupDiagnostics.scala` — Add a diagnostic check for the new mode table if applicable.

**Additional mapping registrations (easy to miss):**
- `graphql/mapping/ExposureTimeModeMapping.scala` — Add `etmMappings(FooLongSlitType, ExposureTimeModeView)` for the science type, and `etmMappings(FooLongSlitAcquisitionType, ExposureTimeModeView)` if the mode has an acquisition ETM row. GNIRS has both. IGRINS-2 (no acquisition) has only the science entry.
- `graphql/mapping/TimeSpanMapping.scala` — Add `timeSpanMappingAtPath(FooLongSlitAcquisitionType / "exposureTime", ...)` for any inline acquisition `TimeSpan` field. Also extend the trait with the mode's view trait.
- `graphql/mapping/WavelengthMapping.scala` — Add `wavelengthMappingAtPath(...)` for every `Wavelength` output field (both non-null and nullable). Extend the trait with the mode's view trait. For **nullable** `Wavelength` fields (e.g. `explicitGratingWavelength`): use the **nullable column** (`wavelength_pm.opt`) as the key (so null DB value → null GraphQL object) and a **non-nullable alias** of the same column (`wavelength_pm`) as the value column.
- `graphql/mapping/LeafMappings.scala` — Add `LeafMapping[FooEnum](FooEnumType)` for every new enum exposed in the schema. Also add the corresponding `lazy val FooEnumType = schema.ref("FooEnum")` in `graphql/BaseMapping.scala`.
- `graphql/mapping/ObservingModeMapping.scala` and `graphql/OdbMapping.scala` — Register the new mode's `ObjectMapping` instances.
- `service/GeneratorParamsService.scala` — Add a `case foo: foo.longslit.Config =>` branch to `toObsGeneratorParams` that constructs the appropriate `InstrumentMode` and `ItcInput`.

## GraphQL Schema Pitfalls (Grackle)

Grackle validates all type references at startup during schema introspection. A type referenced in the schema but not defined causes a `scala.MatchError` with the type name. The service will start but schema fetch will fail.

**How to debug MatchErrors:**

1. The error prints the unknown type name, e.g. `scala.MatchError: StepGuideState`.
2. Search the schema for where it's referenced: `grep -n "StepGuideState" OdbSchema.graphql`.
3. Search for the actual existing type: `grep -n "GuideState\|StepGuide" OdbSchema.graphql`.
4. Fix the wrong name in the GNIRS/new mode section.

**Known type name mistakes made during GNIRS implementation (do not repeat):**

| Wrong name used | Correct name |
|---|---|
| `ExposureTimeModeTimeAndCountInput` | `TimeAndCountExposureTimeModeInput` |
| `ComponentInput` | `OffsetComponentInput` |
| `StepGuideState` | `GuideState` |

**General rule:** Input types follow `{Concept}Input` naming but not always; always verify against existing definitions before writing new schema references.

**`ObjectFieldsBinding.rmap { case List(...) }` is positional.** Field names in the pattern are just labels; Grackle matches by position in the list against the schema's field declaration order. If a new input field is added to a union input (like `ObservingModeInput`) and placed in a different position than the existing binding, the `case List(...)` will silently fail to match (runtime "unhandled case" error). Always make the `case List(...)` order match the schema field order exactly.

## JSON Codecs (`modules/schema/src/main/scala/lucuma/odb/json/`)

Each domain type gets its own file (e.g., `offset.scala`, `wavelength.scala`) with three layers:

| Layer | Purpose | Encoder output |
|---|---|---|
| `DecoderFoo` trait | Shared decoder only | — |
| `QueryCodec` trait | Decoder + rich encoder | All unit representations (µas, mas, arcsec, …) |
| `TransportCodec` trait | Decoder + compact encoder | Microarcseconds only |

`object query extends QueryCodec` and `object transport extends TransportCodec` are the import targets.

**When to use which:**
- `query` — GraphQL API responses (users can request any unit).
- `transport` — DB JSON blobs, inter-service payloads (compact, lossless integer µas).

**Existing codecs and their locations:**

| Type | File | Notes |
|---|---|---|
| `Offset.Component[A]`, `Offset` | `offset.scala` | `offset.µas` is `SplitMono[Offset.Component[A], Long]` |
| `TelescopeConfig` | `stepconfig.scala` | `Encoder[TelescopeConfig]` requires `Encoder[Offset]` as context param |
| `TelescopeConfigAlongSlit` | `telescopeconfigalongslit.scala` | Same query/transport split as offset |
| `Wavelength` | `wavelength.scala` | |

**Import example in a Grackle mapping (GraphQL output):**
```scala
import lucuma.odb.json.offset.query.given
```

**Import example in DB storage (`TelescopeConfigsFormat`):**
```scala
import lucuma.odb.json.offset.transport.given
import lucuma.odb.json.stepconfig.given
import lucuma.odb.json.telescopeConfigAlongSlit.transport.given
```

## Slit Telescope Configs DB Storage

`SlitTelescopeConfigs` is stored in two columns: a discriminant tag (`c_slit_offset_mode`) and a JSON blob (`c_spatial_offsets`).

- `TelescopeConfigsFormat` / `object telescopeConfigs` in `modules/schema/src/main/scala/lucuma/odb/format/TelescopeConfigsFormat.scala` handles the round-trip.
- The JSON blob uses the **transport** codec (integer microarcseconds, no extra unit fields).
- `AlongSlitFormat` — `String ↔ NonEmptyList[TelescopeConfigAlongSlit]`
- `ToSkyFormat` — `String ↔ NonEmptyList[TelescopeConfig]`
- `SlitTelescopeConfigsFormat` — `(SlitOffsetMode, String) ↔ SlitTelescopeConfigs` (dispatches based on mode tag)

## Mode Table Conventions

All mode tables (`t_gmos_north_long_slit`, `t_flamingos_2_long_slit`, `t_igrins_2_long_slit`, `t_gnirs_long_slit`, …) share these conventions:

- **`c_program_id NOT NULL`** — denormalized from `t_observation`. Always populated on INSERT via `SELECT c_program_id FROM t_observation WHERE c_observation_id = $id`. Never passed as input.
- **`c_initial_*` columns** — snapshot of values at creation time; never updated. Used for mode grouping key and revert-to-initial semantics.
- **View computed columns** — suffixed `_default` for pure computed defaults (no COALESCE with explicit); suffixed `_effective` for `COALESCE(explicit, default)`.

**View column type pitfall:** `CREATE OR REPLACE VIEW` cannot change a column's PostgreSQL type. If you need to change a computed column's type (e.g., from `character varying` via COALESCE-with-domain to `text` via bare CASE WHEN), you must `DROP VIEW` first. In migrations, always cast bare string literals to `::varchar` when replacing COALESCE expressions to keep the type stable.

## Grackle Mapping Patterns

- SQL view columns are mapped with `SqlField("fieldName", TableName.ColumnName, key = true/false)`.
- Computed/derived fields (from Scala logic) use `CursorField`.
- JSON-typed columns (wavelengths, angles stored as pm/µas) require an implicit `Encoder`/`Decoder` in scope. Import the right given instance, e.g.: `import lucuma.odb.json.wavelength.query.given`.
- Every type used in the schema (including input types) must either have a registered mapping or be a scalar/enum already known to Grackle.

## Skunk Codec Patterns

When writing a `Fragment` / `encoder` for an INSERT that maps to a table with `c_initial_*` columns, the contramap output must include **both** the regular value and the initial value as separate elements, even when they start identical:

```scala
// Wrong — will fail at compile time with arity mismatch
(oid, grating, prism, camera, fpu, wavelength, filter)

// Right — camera and fpu each appear twice (once for c_camera, once for c_initial_camera)
(oid, grating, prism, camera, camera, fpu, fpu, wavelength, wavelength, filter, filter)
```

## Configuration Requests & `ConfigurationService`

`Configuration.ObservingMode` (from `lucuma-core`) has a variant per mode. `ConfigurationService.scala` matches on `(ObservingModeType, ..., Some(discriminantValue), ...)` tuples from SQL to construct these. When adding a new mode:

- Add the match case in both occurrences of `selectMany` (there are two — one for a single obs, one for multiple).
- The JSON codec in `configurationrequest.scala` must encode and decode the new variant; the discriminant field is the mode-specific key (e.g., `"gnirsLongSlit"`).

## ETM (ExposureTimeMode) Convention

"Acquisition config" means the non-ETM acquisition fields (filter, read mode, coadds, sky offset). "Acquisition ETM" is the exposure time + count row in `t_exposure_time_mode`. The two are independent: a mode can store acquisition config inline in its own table and still keep the acquisition ETM in `t_exposure_time_mode`.

| Mode | Acquisition config | Acquisition ETM | Science ETM |
|---|---|---|---|
| GMOS North/South LongSlit, Flamingos-2 LongSlit | (none inline; comes from observation/defaults) | `t_exposure_time_mode` (role=acquisition) | `t_exposure_time_mode` (role=science) |
| GNIRS LongSlit | Inline columns in `t_gnirs_long_slit`: `c_acq_type`, `c_acq_coadds`, `c_acq_filter`, `c_acq_sky_offset_p`, `c_acq_sky_offset_q` (the two components together form a single `Option[Offset]` sky offset, enforced by a both-or-neither CHECK) | `t_exposure_time_mode` (role=acquisition) | `t_exposure_time_mode` (role=science) |
| IGRINS-2 LongSlit | (no acquisition sequence) | — | `t_exposure_time_mode` (role=science) |
| GHOST IFU | — | — | Two rows (red + blue) in `t_exposure_time_mode` |

The `check_etm_consistent` trigger function must be updated for each new mode.

## Docker Test Image

Tests use Testcontainers (not docker-compose) to build a custom PostgreSQL image from `modules/service/src/Dockerfile`. The Dockerfile runs all Flyway migrations at image-build time. If a migration fails, the build fails with exit code 3 and Docker caches the failure.

**If tests fail with `ContainerFetchException` / exit code 3:**
1. Run `docker builder prune -f` to clear the stale build cache.
2. Then rerun; Testcontainers will rebuild the image fresh.

**Migration type stability:** `CREATE OR REPLACE VIEW` cannot change a column's PostgreSQL type — the Docker build will fail even if the dev server accepted the migration (because the dev server had an older view already in place). Always use `DROP VIEW; CREATE VIEW` when changing column types, and cast bare string literals to `::varchar` to match domain-derived column types.
