# Observation workflow states and transitions

Source of truth: `ObservationWorkflowService.workflowStateAndTransitions`
(`modules/service/src/main/scala/lucuma/odb/service/ObservationWorkflowService.scala`).

Black edges are the ones reachable through the `setObservationWorkflowState`
mutation — they are exactly the contents of `allowedTransitions`. Every other
colour is a re-derivation that happens on its own, distinguished by who triggers
it: blue for the PI editing the observation, green for a staff approval, red for
a staff denial.

![Observation workflow states](observation-workflow.svg)

The four panels are, top to bottom: science observations in a proposal, science
observations in a program, program-level calibrations, and per-observation
calibrations. Each is described below.

## Main lifecycle

`Inactive` returns to `executionState.getOrElse(validationStatus)` — so if
execution has already begun it comes back as `Ongoing`/`Completed`, otherwise to
whatever the validation state recomputes to, which need not be the state it left.

### Defined and Unapproved

`Unapproved` is one state standing for four validation codes
(`validateConfigurations`): no request exists, all requests denied, or some
request still pending. There is no separate reviewed/unreviewed state — approval
lives entirely in `ConfigurationRequestStatus` — so each direction between
`Defined` and `Unapproved` has both a PI route and a staff route:

| Edge | Trigger |
|---|---|
| `Defined -> Unapproved` (blue) | PI changes the configuration to one no approved request covers |
| `Defined -> Unapproved` (red) | staff withdraws the approval that was holding it `Defined` |
| `Unapproved -> Defined` (blue) | PI changes the configuration to one an approved request already covers |
| `Unapproved -> Defined` (green) | staff approves the pending request |

`Defined` requires at least one request with status `Approved`. Note that denying
a request that was merely *pending* changes the validation code but not the
state — the observation stays `Unapproved` — so the red edge specifically means
revoking an approval, not the everyday act of rejecting a request.

## Targets of Opportunity

An observation whose `tooActivation` is anything but `NONE` waits for an alert
rather than for the queue, and its `Ready` state works differently from every
other observation's: it is **derived from an accepted ToO trigger**, not stored
in `c_workflow_user_state`.

| Trigger | Observation |
|---|---|
| none, or `DENIED` / `WITHDRAWN` | at most `Defined` |
| `ACCEPTED` | `Ready`, provided validation is otherwise clean |

Consequences, all of which fall out of deriving rather than storing:

- `Defined -> Ready` and `Ready -> Defined` are **not** offered in
  `allowedTransitions` for a ToO observation, for any role. Accepting or
  withdrawing the trigger is the only lever. Offering `Ready` would write a user
  state that the ToO branch of `isReady` never reads, so the mutation would
  report success and change nothing.
- Validation still outranks the trigger. A ToO observation edited into an
  invalid state drops out of `Ready` exactly like any other, because
  `userStatus` checks `validationStatus` before consulting the trigger at all.
- `Inactive` still overrides, and clearing it returns the observation to `Ready`
  if the trigger is still accepted — see `stateWithoutInactive`. Marking a ToO
  observation inactive is not a withdrawal.

`requestTooTrigger` refuses unless the observation declares a non-`NONE`
activation, holds no opportunity placeholder in its asterism, and is in state
exactly `Defined`. That last check reads the *cached* state from
`t_obscalc.c_workflow_state`: it is a guard on a human action rather than the
thing enforcing the invariant, so agreeing with what the UI is displaying is
worth more than a fresh computation. Even a stale `Defined` cannot produce a
wrongly-`Ready` observation, because validation is recomputed when the trigger
is accepted.

Withdrawal is allowed while the trigger is `REQUESTED` or `ACCEPTED`, up until
the observation begins executing — acceptance is not the point of no return, the
first non-slew execution event is. That is read live from
`v_observation.c_execution_state`, not from the obscalc cache.

An opportunity target is a placeholder for a target not yet found, so it is a
`ConfigurationError` (hence `Undefined`) both when the observation declares no
ToO activation and when its trigger has already been accepted — the latter
being a backstop against swapping a placeholder back in after the observation
has gone `Ready`.

## Calibrations

Two rules apply to **every** calibration role, whatever its kind:

- A calibration never runs the validation pipeline. `validationStatus` is forced
  to `Defined` as soon as `calibrationRole` is set, so a calibration is never
  `Undefined` or `Unapproved`.
- Calibration programs have `ProgramType.hasProposal == false`, so the
  `Defined -> Ready` gate passes without an accepted proposal.

Beyond that the five roles split into two groups:

| Role | Group | Lifecycle |
|---|---|---|
| `Photometric`, `SpectroPhotometric`, `Twilight` | program-level | the generic lifecycle above, entered at `Defined` |
| `Telluric`, `DaytimePinhole` | per-observation | inherits its science observation's user state; see below |

### Program-level calibrations

`Photometric`, `SpectroPhotometric`, and `Twilight` fall through to the generic
`else` branch of `allowedTransitions`, so they run the full lifecycle — they are
simply never gated on validation or on proposal acceptance. `Defined` is both the
entry point and the floor: because `validationStatus` is pinned to `Defined`, the
two edges that return "to the validation state" (`Ready -> validationStatus` and
`Inactive -> validationStatus`) always land back on `Defined`.

Compared with the main lifecycle, only the left-hand side changes: `Undefined`
and `Unapproved` are unreachable, and `Defined -> Ready` carries no
proposal-acceptance condition. Everything from `Ready` rightwards is identical,
including the staff-and-visitor-mode restriction on `Ready <-> Ongoing`.

### Per-observation calibrations

Tellurics and daytime pinholes are auto-created alongside their science
observation and normally have **no** transitions of their own — they inherit the
science observation's user state. SC-8458 carves out one exception, for tellurics
only (see `../../../docs/adr/0008-telluric-decline-override.md`).

A telluric that is `Inactive` only because its science observation is inactive
offers no transitions — there is no override to clear. Both calibration carve-outs
apply only while `state <= Ready`; once execution begins the generic rules resume.

## Transition conditions

| Transition | Requires |
|---|---|
| `Defined -> Ready` | not an exchange observation, no opportunity target, not a ToO observation (those go `Ready` via an accepted trigger instead), and (proposal accepted or program has no proposal — `hasProposal` is true only for `Science`, `Keck`, `Subaru`) |
| `Ready -> Defined` | not a ToO observation; withdraw the trigger instead |
| `Ready -> Ongoing`, `Ongoing -> Ready` | visitor observing mode **and** staff access or above |
| `Completed -> Ongoing` | execution state was explicitly declared complete, not naturally complete |
| `Telluric -> Inactive` | calibration role is `Telluric` and `state <= Ready` |
| `Telluric Inactive -> inherited` | the telluric's own `c_workflow_user_state` is `Inactive` |

Exchange observations (Keck/Subaru) run off-Gemini and have no
`Ready`/`Ongoing`/`Completed` lifecycle at all; `Inactive` is their only
transition out of `Defined`.

## DB Columns

The state is not stored as a single column. It is computed each time, from three
independent sources, in this precedence order:

| Kind | Members | Where it comes from |
|---|---|---|
| `ExecutionState` | `Ongoing`, `Completed` | `c_declared_execution_state`, else the generator's execution state |
| `UserState` | `Inactive`, `Ready` | `c_workflow_user_state` — except that a ToO observation's `Ready` comes from an accepted row in `t_too_trigger` instead |
| `ValidationState` | `Undefined`, `Unapproved`, `Defined` | validation codes computed from the observation |

Execution wins over user state, which wins over validation — except that a
validation error suppresses a stored `Ready` (but never a stored `Inactive`).

## Regenerating the diagram

There is nothing to regenerate: `observation-workflow.svg` is hand-authored and
is its own source. It is deliberately not produced from mermaid or any other
text format — the readability depends on hand-placed nodes (`Inactive` stacked
above each state, `Unapproved` dropped below the row, the legend parked in the
gap), and auto-layout engines will not reproduce that.

To change it, edit the SVG directly. The header comment lists the shared column
positions and the per-panel `y` bands; keeping equivalent states on the same
column is what makes the four panels comparable at a glance.

To preview a change on macOS:

```bash
qlmanage -t -s 1520 -o /tmp docs/observation-workflow.svg   # writes /tmp/observation-workflow.svg.png
```

`qlmanage` crops to a square rather than fitting, so on a wide diagram it will
cut off the right-hand side. To see the whole thing, temporarily pad the
`viewBox`/`height` to a square (`0 0 1520 1520`) in a scratch copy, or just open
the file in a browser, which honours the real aspect ratio.

When adding a panel, extend the root `viewBox` and `height` to match; everything
else is absolute coordinates and will stay where it is.
