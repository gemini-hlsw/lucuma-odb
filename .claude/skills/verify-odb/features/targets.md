# Targets

A PI adds a sidereal target, with coordinates, proper motion and a band-normalized point-source profile, to their program.

## Sub-features

- `target-create`: create a sidereal target in a program.
- `target-read`: the target is listed in the program and readable by id.
- `target-denied`: a PI who does not own the program cannot add targets to it.

## How to get to it (user POV)

- `createTarget` mutation, which is Explore's target editor.
- `target(targetId:)` and `targets(WHERE:)` queries.

## Driving it with odb.sh

Preconditions:

- A program `$P` owned by `pi` (see programs-and-access `program-create`).

- **Create.** Run `T=$($S gql pi @$Q/create-target.graphql "{\"p\":\"$P\"}" | jq -r .data.createTarget.target.id)`. The response has `target.id` and name `V1647 Orionis`.
- **Read back.** Run `$S gql pi "query(\$t: TargetId!) { target(targetId: \$t) { id name sidereal { ra { hms } dec { dms } } } }" "{\"t\":\"$T\"}"`. The coordinates are `05:46:13.137000` and `-00:06:04.890000`.
- **Other PI denied.** Run `$S gql pi@0x2001 @$Q/create-target.graphql "{\"p\":\"$P\"}"`. Errors show `odb_error.tag == "not_authorized"` for `u-2001`, and no target is created.
- **Stored.** Run `$S psql -c "select c_target_id, c_name, c_program_id from t_target where c_program_id='$P'"`. Only the owner's target is there.

## Gotchas

- `queries/create-target.graphql` uses V = J = 10 mag on purpose. The original test value, V = 18.1, is too faint for the ITC journey's S/N target.
- Coordinates come back normalized, so compare values rather than strings when formats differ.
