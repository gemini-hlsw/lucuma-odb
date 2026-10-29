# Observations per night for groups

## Summary

Some programs need a group's observations to be taken in batches: at least
some number on any night the group is observed, and/or no more than some number
on any one night.  Add two optional constraints to groups:

- `minimumObservationsPerNight`: on any night when the group is observed, at
  least this many of its observations must be completed that night.
- `maximumObservationsPerNight`: on any night, at most this many of its
  observations may be completed.

These are independent of `minimumInterval` and `maximumInterval`, which limit
the time between observations.  The new fields limit how many observations
fall on each night.  A group must satisfy all of its constraints.

(Science's request called these `Nmin` and `Nmax`, and limited them to AND
groups.  They apply equally to OR groups once completions are counted as
below, so this spec does not restrict them.  In an AND group every completion
counts toward the group, so there are no extras and the rules below reduce to
the simple case.)

## Definitions

- **Counted observations**: the group's direct child observations that are not
  inactive (unless complete) or deleted, nor declared complete without ever
  having been observed.  A completion falls on the night of its completion
  time; a visitor observation's completes where its time charge ends.  The
  group may contain only observations and
  per-observation calibration groups (see Validation).
- **Per-observation calibration group**: the system group the ODB wraps around
  a science observation that needs its own calibrations (F2 and GNIRS
  tellurics, GNIRS daytime pinhole flats).  It sits where the science
  observation was, so forbidding it would rule out per-night limits for those
  instruments, and would make calibration generation fail.  It counts as one
  observation that completes when the group completes, on that night.  The
  calibrations inside it are not counted separately.  Calibration observations
  are never direct children of a user group otherwise.
- **Counted completions**: every completion of a counted observation, on any
  night up to and including the night the group completes.  This includes an
  OR group's extras: completions beyond what the group requires, such as a
  fourth observation in a 3-of-5 group, or an ordered group's elements after
  the first `required`.  Completions on later nights are ignored.  This differs
  from group progress's count toward `required`, which leaves extras out.
- **Night**: the observing night at the observation's site, as group progress
  already defines it for `sameNight`.
- **An observation's night**: the night on which it completes.  An observation
  whose visits span more than one night counts only toward the night it
  completes.  Group progress already assigns nights to completed observations
  this way.
- **Observed night**: a night with at least one counted completion.

## Semantics

- Both limits apply to the number of counted completions on a night.  Extras
  count toward both limits on the night the group completes.  For example, a
  3-of-5 group with a minimum of 2 may be observed as 2 + 2.  With a maximum of
  2, a night on which the third and fourth observations both complete is over
  the limit, even though the group is complete.
  - We count extras because ignoring them makes a legal first night a trap.  A
    3-of-5 group observed 2 on its first night can only finish with a night of
    2, and if only one of those counted, that night would be short.
  - After the night the group completes, nothing counts.  Otherwise a single
    extra observed later would be a short night.
- A group with enough completions stays `ONGOING` while the night in progress
  is short of the minimum and an observation remains that could make it up.
  Its window ends with that night.  If the night ends short, or no observation
  remains to make it up, that is `OBSERVATIONS_PER_NIGHT_MINIMUM`.
- Only observed nights are constrained.  A night on which nothing in the group
  is observed never violates either limit.  The minimum does **not** mean
  "observe something every night."
- An unset minimum means no minimum.  Because only observed nights count, this
  is the same as a minimum of 1.
- An unset maximum means no maximum.
- No night is exempt from the minimum, including the last one.  With 5
  observations and minimum = maximum = 2, the group cannot be completed (see
  No infeasibility prediction).

## API

`Group` gains:

```graphql
"""
On any night when at least one of this group's observations completes, at
least this many must complete that night.  Nights on which none complete are
unconstrained.  Null means no minimum.  Only valid for groups whose elements
are all observations.  Calibration observations are not counted.  Every other
completion is, including an OR group's extras, through the night the group
completes.
"""
minimumObservationsPerNight: PosShort

"""
No more than this many of this group's observations may complete on any one
night.  Null means no maximum.  Only valid for groups whose elements are all
observations.  Counted as for `minimumObservationsPerNight`.
"""
maximumObservationsPerNight: PosShort
```

`GroupPropertiesInput` gains the same two fields, both nullable.  In an edit,
null clears the field.

## Validation (mutation errors)

The following are rejected when a group is created or edited:

1. A value less than 1 (enforced by `PosShort`).
2. A minimum greater than the maximum.
3. Either field set together with `sameNight: true`.  This parallels the
   existing exclusivity of `sameNight` and `maximumInterval`.
4. Either field set on a group with a present subgroup among its elements,
   other than a per-observation calibration group.  Conversely, creating such
   a subgroup in, or moving one into, a group that has either field set is
   rejected.  Deleted subgroups are ignored.

## Group progress violations

Each constraint is evaluated independently.  Any violation makes the group
`UNSCHEDULABLE`.  We deliberately err on the strict side, because the
upcoming group-progress work makes violations user-dismissable.  There is no
analysis across constraints.  For example, a minimum per night of 2 combined
with a `minimumInterval` longer than a night is not flagged in advance; it
surfaces as `OBSERVATIONS_PER_NIGHT_MINIMUM` when the first such night ends
short.

New `GroupConstraintViolation` values:

| Violation | Condition |
|---|---|
| `OBSERVATIONS_PER_NIGHT_MINIMUM` | An observed night ended with fewer than the minimum completed. |
| `OBSERVATIONS_PER_NIGHT_MAXIMUM` | More than the maximum completed on one night. |
| `OBSERVATIONS_PER_NIGHT_MULTIPLE_SITES` | The counted observations are at more than one site, so "night" is ambiguous.  This parallels `SAME_NIGHT_MULTIPLE_SITES`. |

### No infeasibility prediction

Whether the nights still to come can satisfy the limits is deliberately not
predicted.  A group that can never finish, such as 5 observations with
minimum = maximum = 2, or one observation with a minimum of 2, is not flagged
in advance.  It becomes `OBSERVATIONS_PER_NIGHT_MINIMUM` once a night comes up
short: when that night ends, or earlier, once the group has enough
completions and no observation remains that could make the night up.  The
scheduler sees such a group's window close regardless.  (An earlier design
flagged `OBSERVATIONS_PER_NIGHT_INFEASIBLE` up front; the arithmetic, with
extras and the night in progress, was the riskiest part of the evaluator for
the least benefit.)

### Group window

`GroupWindow` should also reflect the new limits:

- **While a night is in progress with fewer than the minimum completed**: the
  window ends at the end of that night, as for a same-night group.  This
  applies even once the group has enough completions (see Semantics).
- **Once a night reaches the maximum**: the window starts at the start of the
  next night.

These combine with the interval limits: the latest start and the earliest end
win.

## Out of scope / dependencies

- **Dismissing violations** belongs to the group-progress work.  A dismissal
  must name the specific occurrence (for example, a particular short night),
  not the violation type as a whole.  Otherwise, dismissing one short night
  would hide every later one.
- **Subgroups as elements.** We can relax this later without breaking
  anything.  Relaxing it would require deciding which night a subgroup counts
  toward.
- **Groups spanning sites.** These are not supported; see
  `OBSERVATIONS_PER_NIGHT_MULTIPLE_SITES`.
- **Explore UI and scheduler** support are separate tasks.

## Implementation notes

- Add a migration with two nullable `int2` columns on `t_group`, plus `CHECK`
  constraints on the row:
  - values > 0
  - min ≤ max
  - `c_same_night = false OR (both null)`
- Enforce the no-subgroups rule (validation rule 4) with a deferred constraint
  trigger, not a `CHECK`.  It spans the group and its children; see the
  cross-table invariant section of `modules/CLAUDE.md`.  The trigger is the
  only implementation of the rule, with no duplicate check in Scala:
  - It re-reads the row at commit rather than trusting `NEW`, so the order of
    statements within a mutation doesn't matter.
  - It also fires on `c_system` and `c_calibration_roles`, so turning a
    per-observation calibration group into an ordinary group is caught.
  - The `createGroup`, `updateGroups` and `cloneGroup` mutations report its
    message as an `INCONSISTENT_GROUP` error, as they already do for the other
    deferred group triggers.
- `GroupPropertiesInput`: add the fields, the min ≤ max check, and the
  `sameNight` exclusivity, next to the existing check against
  `maximumInterval`.
- `GroupProgressEvaluator`: leaf elements already carry their completion
  night and site.  The per-night counts come from grouping by night every
  completion of the group's viable elements, cut off at the end of the night
  the group completes.
- `GroupProgress.Ongoing` in lucuma-core is documented as "fewer elements
  complete than required".  It needs the exception for a short night in
  progress.
- `sameNight` is documented as "only valid for AND groups", but that rule does
  not appear to be enforced.  The same counting would let it apply to OR groups
  too.  That is a separate decision, outside this task.
