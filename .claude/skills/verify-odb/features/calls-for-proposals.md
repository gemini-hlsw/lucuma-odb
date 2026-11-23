# Calls for proposals

Staff open a call for proposals for a semester. PIs cannot create one.

## Sub-features

- `cfp-create-staff`: staff create a regular-semester call with active dates.
- `cfp-create-pi-denied`: a PI is refused with `not_authorized`.
- `cfp-list`: the new call appears in `callsForProposals`.

## How to get to it (user POV)

- `createCallForProposals` mutation, which is the staff admin tooling.
- `callsForProposals` query, which is what PIs see when they pick a call for a proposal.

## Driving it with odb.sh

Preconditions:

- Baseline from the index. No call exists for semester `2027A`.

- **PI denied.** Run `$S gql pi @$Q/create-cfp.graphql '{"semester":"2027A","start":"2027-02-01","end":"2027-07-31"}'`. Errors show `odb_error.tag == "not_authorized"` and `data` is null.
- **Staff create.** Run the same as `staff`. The response has `callForProposals.id` (for example `c-100`), semester `2027A`, and type `REGULAR_SEMESTER`.
- **Listed.** Run `$S gql pi '{ callsForProposals { matches { id semester } } }'`. The new call is visible to PIs.
- **Stored.** Run `$S psql -c "select c_cfp_id, c_semester from t_cfp"`. One row for `2027A`.

## Gotchas

- Run the PI attempt first. Then a success can only come from staff, and the denial proves the rule rather than a missing precondition.
- The `t_cfp` column names are from the migrations as of 2026-11. If `psql` errors, run `\d t_cfp`.
