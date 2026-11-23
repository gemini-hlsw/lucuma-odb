# Observations

A PI creates an observation with constraints, spectroscopy science requirements and a GMOS North long-slit observing mode on one of their targets.

## Sub-features

- `obs-create`: create a GMOS North long-slit observation on a target.
- `obs-read`: the observation is readable with its mode and target.
- `obs-denied`: another PI cannot read it.

## How to get to it (user POV)

- `createObservation` mutation, which is Explore's "new observation" plus its configuration.
- `observation(observationId:)` and `observations(WHERE:)` queries.

## Driving it with odb.sh

Preconditions:

- Program `$P` and target `$T` owned by `pi` (see programs-and-access and targets).

- **Create.** Run `O=$($S gql pi @$Q/create-gmosn-longslit-obs.graphql "{\"p\":\"$P\",\"t\":\"$T\"}" | jq -r .data.createObservation.observation.id)`. The response has `observation.id`.
- **Read back.** Run `$S gql pi "query(\$o: ObservationId!) { observation(observationId: \$o) { id observingMode { mode } targetEnvironment { asterism { id } } } }" "{\"o\":\"$O\"}"`. The mode is `GMOS_NORTH_LONG_SLIT` and the asterism contains `$T`.
- **Other PI denied.** Run the same query as `pi@0x2001`. It returns `observation: null`.
- **Stored.** Run `$S psql -c "select c_observation_id, c_program_id, c_observing_mode_type from t_observation where c_observation_id='$O'"`. One row.

## Gotchas

- Workflow state reads `UNDEFINED` with no validation errors. That is because the obscalc worker is not running, and it does not mean the observation is broken. Do not use workflow state as proof here.
- The query's wavelengths are 650 nm, inside the r′ filter. At 500 nm, which the Scala tests use with a stub ITC, the real ITC finds no signal.
