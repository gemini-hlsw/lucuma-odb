# ITC and sequence

For a fully defined observation, the ODB asks the ITC for exposure times and generates the execution sequence: calibration steps plus science steps.

## Sub-features

- `itc-result`: `observation.itc` returns the selected science exposure time and count.
- `sequence-generate`: `executionConfig` returns the next science atom, with GCAL flat and arc steps and a science step whose exposure matches the ITC.
- `itc-error`: a configuration with no signal reports an `itc_error` instead of a sequence.

## How to get to it (user POV)

- `observation { itc { ... } }`, which is Explore's ITC panel.
- `executionConfig(observationId:)`, which is the sequence view and what Observe reads.

## Driving it with odb.sh

Preconditions:

- An observation `$O` created from `queries/create-gmosn-longslit-obs.graphql` on a target from `queries/create-target.graphql`.
- Dev ITC is reachable. `$S doctor` shows `itc: ok` in its `ready` line.

- **ITC and sequence.** Run `$S gql pi @$Q/observation-itc-sequence.graphql "{\"o\":\"$O\"}"`. `itc.itcType` is `SPECTROSCOPY` and `spectroscopyScience.selected.exposureTime.seconds` is about 16. `executionConfig.gmosNorth.science.nextAtom.steps` is two `GCAL` steps followed by a `SCIENCE` step with the same exposure. `errors` is absent.
- **No-signal error.** Create an observation whose S/N wavelength is 500 nm (edit a copy of the query) and run the same query. The response carries `odb_error.tag == "itc_error"` with `Insufficient signal`, and `executionConfig` is `null`.

## Gotchas

- This calls **real dev ITC**, so results can change when dev ITC is redeployed. Assert on shape and on agreement between ITC and the science step, not on exact seconds.
- The ODB's service token is minted locally, and dev ITC accepted it on 2026-11-23. If ITC starts answering 401, that acceptance changed. Point `VERIFY_ODB_ITC_ROOT` at a local ITC (`sbt itcService/reStart`) instead.
