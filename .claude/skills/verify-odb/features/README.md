# ODB verification map

This is the maintained source for verifying user-facing ODB behavior through its GraphQL API. Read this index first, then use the matching feature file as the recipe. `S` below means `.claude/skills/verify-odb/scripts/odb.sh` and `Q` means `.claude/skills/verify-odb/queries`.

## Baseline preconditions

- `$S build` has run since the last code change, and `$S up` finished with `auth ok`.
- `$S doctor` shows the `build` commit equal to `HEAD`.
- Every `up` starts from an empty database: migrations only, plus the calibration programs they seed. So ids like `p-102` and `t-194` vary. Always read ids from responses, never hard-code them.
- Never drive an ODB this run did not start. `doctor` must report `port ok` and `database ok`.

## Driving conventions

- Act as the role that does it in production: PIs create programs, targets and observations, and staff create calls for proposals.
- Use `<kind>@<id>` for a second user of the same kind, such as `pi@0x2001`.
- Pass variables as JSON. Capture ids with `jq -r`.
- Export `EVIDENCE=$($S evidence <feature-id>)` before the first call of a journey.

## Proof and skip reporting

- Proof is the evidence folder: every request/response pair plus `psql` reads of the stored rows.
- A mutation is proven only by a second read-only view: a re-query, or a `psql` select.
- An access rule is proven only when both the allowed and the denied user were exercised.
- Report an unreachable journey with the command, the response, and the unmet precondition. Never report it as verified through a different path.

## Feature entry contract

Each feature file starts with an H1 title and one paragraph describing the user-visible behavior. It then uses exactly four H2 sections in this order: `Sub-features`, `How to get to it (user POV)`, `Driving it with odb.sh`, `Gotchas`.

## Features

- [Programs and access](./programs-and-access.md): create a program as PI, and see who can read it (PI, other PI, guest, NGO, staff, admin).
- [Targets](./targets.md): create a sidereal target with a source profile inside a program.
- [Observations](./observations.md): create a GMOS North long-slit observation on a target.
- [ITC and sequence](./itc-and-sequence.md): ITC exposure time and the generated execution sequence for an observation (needs dev ITC).
- [Calls for proposals](./calls-for-proposals.md): staff-only creation of a call for proposals, which PIs are refused.
