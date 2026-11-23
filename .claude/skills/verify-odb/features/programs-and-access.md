# Programs and access

A PI creates a science program and becomes its PI. Other PIs and guests cannot see it. Staff and admin can see every program.

## Sub-features

- `program-create`: a PI creates a named program and is recorded as its PI.
- `program-read-owner`: the owning PI reads it back.
- `program-read-denied`: another PI, a guest, and an NGO user without an allocation get `null`.
- `program-read-staff`: staff and admin read it, and find it with `programs(WHERE: ...)`.

## How to get to it (user POV)

- `createProgram` mutation, which is Explore's "new program".
- `program(programId:)` query, and the `programs(WHERE:)` list query.

## Driving it with odb.sh

Preconditions:

- Baseline from the index. No program named `Verify program` exists yet. A fresh `up` guarantees that.

- **Create.** The PI creates it. Run `P=$($S gql pi @$Q/create-program.graphql '{"name":"Verify program"}' | jq -r .data.createProgram.program.id)`. The response has `program.id` and `pi.user.id == "u-1001"`.
- **Owner reads.** Run `$S gql pi @$Q/program.graphql "{\"p\":\"$P\"}"`. It returns the program with the same name.
- **Other PI denied.** Run `$S gql pi@0x2001 @$Q/program.graphql "{\"p\":\"$P\"}"`. It returns `{"data":{"program":null}}`.
- **Guest and NGO denied.** Run the same with `guest` and with `ngo:US`. Both return `program: null`.
- **Staff and admin read.** Run the same with `staff`. It returns the program. Run `$S gql admin '{ programs(WHERE: {name: {EQ: "Verify program"}}) { matches { id } } }'`. The list contains `$P`.
- **Stored.** Run `$S psql -c "select p.c_program_id, p.c_name, u.c_user_id, u.c_role from t_program p join t_program_user u using (c_program_id) where p.c_program_id='$P'" > $EVIDENCE/db-program.txt`. It shows one row with `u-1001 | pi`.

## Gotchas

- A denied *read* returns `program: null`, not an error. Assert on the null.
- If a token's JSON is malformed, the API answers `Access denied`, which looks like a permission result but is not one. Check `odb.log` for `JWT validation failed` before treating "Access denied" as a finding.
- `t_program` has no PI column. The PI lives in `t_program_user` with `c_role = 'pi'`.
