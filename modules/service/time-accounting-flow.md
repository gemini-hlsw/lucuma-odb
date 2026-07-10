# Time Accounting Flow

## Overview

Time accounting is the record of how much observing time an execution consumed
and how it was charged. It is computed **per visit** and stored on `t_visit`
(`c_raw_*` and `c_final_*` interval columns), plus a set of discount rows in
`t_time_charge_discount`. It is *derived* data: a function of the visit's
execution events, its datasets' QA state, manual staff corrections, and the
observing night.

It is recomputed **asynchronously by the obscalc worker** rather than inline
when events arrive. An execution event only marks the affected visit dirty; the
worker picks the observation up and recomputes the dirty visits. This keeps the
event-ingestion path (from the "Observe" application) cheap.

See [`obscalc-flow.md`](../obscalc/obscalc-flow.md) for the worker/daemon
machinery this rides on.

## Where it lives

| Concern | Location |
|---|---|
| Compute a visit's invoice, write `t_visit` + discounts | `TimeAccountingService.update(visitId)` |
| Recompute all *dirty* visits of an observation | `TimeAccountingService.updateAll(observationId)` |
| Manual staff correction | `TimeAccountingService.addCorrection(visitId, …)` (synchronous) |
| Record an execution event (marks a visit dirty) | `ExecutionEventService` insert methods |
| Drive the recompute in the worker | obscalc daemon `Main.scala` `calcAndUpdateStream` |
| Retry decision when a recompute fails | `ObscalcService.storeResult` |
| Triggers, markers, procedures | migration `V1210__async_time_accounting.sql` |

## What causes time accounting to update

**Exactly two things change a visit's charge**, and each is scoped to a single
visit:

1. **An execution event** is recorded for the visit (`t_execution_event`, which
   has `c_visit_id NOT NULL`).
2. **A dataset's QA state** crosses `Pass`/null ↔ `Fail`/`Usable`
   (`t_dataset`, also `c_visit_id NOT NULL`) — this adds or removes a QA
   discount for the visit that contains the dataset.

Both go through `invalidate_visit_time_accounting(visit_id, observation_id)`,
which:

- `UPDATE t_visit SET c_ta_invalidation = now()` for **that visit only**, and
- `CALL invalidate_obscalc(observation_id)` so the worker picks the observation
  up (and the obscalc digest/workflow refresh too).

Manual corrections are handled **synchronously** in `addCorrection` (staff, rare)
and do **not** go through this path — they write the correction row and adjust
`t_visit` immediately.

## The per-visit dirty markers

`t_visit` carries two timestamps:

| Column | Meaning |
|---|---|
| `c_ta_invalidation` | Bumped to `now()` when this visit is invalidated (event / QA change). |
| `c_ta_update` | Set to the invalidation value that was just recomputed. |

A visit is **dirty** iff `c_ta_invalidation > c_ta_update`.
`updateAll(observationId)` selects only the observation's dirty visits,
recomputes each with `update(visitId)`, and sets that visit's `c_ta_update` to
the invalidation timestamp it observed.

Existing visits (at migration time) get equal default timestamps → not dirty →
never recomputed until a genuine change occurs.

## Why NOT recompute otherwise-unchanged visits

This is the central design invariant, and it is deliberate.

Time accounting is derived, but its stored values are also **acted on by
humans**: staff review the automated numbers and enter manual corrections, and a
program's charged time is a real accounting figure. Consider:

1. Visit V1 executes early in the semester; its charge is computed and staff add
   manual correction entries after reviewing it.
2. Months pass. The time-accounting **algorithm is changed** in a deploy.
3. A new visit V2 of the *same observation* begins executing.

If recomputing were scoped to the *observation* (recompute-all-visits), every
event in V2 would re-derive V1 as well — under the **new** algorithm. The
correction rows survive (they're reapplied from `t_time_charge_correction`), but
the base they sit on drifts, so V1's stored final charge silently changes away
from the value staff reviewed and approved. That is worse than losing the
corrections outright: it looks authoritative but no longer matches the human
decision.

Scoping the recompute to the **visit that actually changed** prevents this: V2's
events touch only V2; V1 is inviolate unless *it* receives an event or a QA
change. Recompute is fine when a human is actively making a relevant change
(even a post-deploy QA correction re-prices with the new algorithm — someone is
in the loop expecting it); the danger is only an *unrelated* trigger silently
picking up an algorithm change.

Two corollaries fall out of per-visit scoping:

- **Unrelated obscalc invalidations don't touch time accounting.** A target/mode
  edit, or a migration that blanket-sets observations to `pending`, invalidates
  obscalc but marks no visit dirty, so `updateAll` is a no-op. (This is why the
  markers are on `t_visit`, not `t_obscalc`.)
- **QA changes are handled correctly** even on an old, closed visit, because the
  QA trigger marks *that* visit dirty.

## Flow

```mermaid
sequenceDiagram
    participant Obs as Observe app
    participant Event as ExecutionEventService
    participant DB as PostgreSQL
    participant Trig as event_time_accounting_invalidate()
    participant Daemon as Obscalc Daemon
    participant TA as TimeAccountingService.updateAll
    participant Calc as ObscalcService.calculateAndUpdate

    Obs->>Event: addStepEvent / addDatasetEvent / …
    Event->>DB: INSERT t_execution_event   (cheap; no recompute)
    DB->>Trig: AFTER INSERT
    Trig->>DB: UPDATE t_visit SET c_ta_invalidation = now()  (that visit)
    Trig->>DB: CALL invalidate_obscalc(observation)  → t_obscalc 'pending'
    DB-->>Daemon: NOTIFY ch_obscalc_update (pending)
    Daemon->>TA: updateAll(oid) — own transaction
    TA->>DB: recompute each DIRTY visit; set its c_ta_update
    Daemon->>Calc: calculateAndUpdate(oid) — ITC / digest / workflow
    Calc->>DB: storeResult → 'ready' (or 'retry' if a visit is still dirty)
```

Key point: `updateAll` runs in **its own transaction, before and independent of**
`calculateAndUpdate`. Time accounting is pure DB work; the ITC/digest calculation
can hit a remote service and fail. Keeping them separate means an ITC failure can
never stall or revert a time-accounting recompute.

## Interaction subtleties

### Retry when a recompute fails

`updateAll` is best-effort in the daemon (errors are logged and swallowed). If it
fails, the visit stays dirty (`c_ta_update` was never advanced). To avoid
orphaning that dirty state, `storeResult` checks whether **any** visit of the
observation is still dirty when the obscalc calculation finishes, and if so sets
the entry to `retry` (with the normal exponential backoff) instead of `ready` —
so the poll picks it up again and `updateAll` is retried.

### Failure vs. concurrent event — retry vs. pending

`storeResult` decides the final state in this order:

```
if c_last_invalidation changed since pickup      → pending   (re-invalidated during calc)
else if any visit still dirty                    → retry     (a recompute failed)
else                                             → ready
```

The ordering matters. A real event bumps **both** the visit's
`c_ta_invalidation` *and* `t_obscalc.c_last_invalidation` (the latter via
`invalidate_obscalc`). So an event that arrives *during* the calculation trips
the `pending` check first — it is correctly treated as "re-invalidated", not as a
failure. The `retry` branch is reached only when a visit is dirty **and**
`c_last_invalidation` is unchanged, which happens exclusively when `updateAll`
itself failed.

### Race safety

`updateAll` records `c_ta_update` = the `c_ta_invalidation` value it *observed*,
never `now()`. So if a new event bumps `c_ta_invalidation` while a recompute is
in flight, the visit is left dirty for the next pass rather than being marked
clean at a stale value — no lost update, regardless of commit order.

### No feedback loop

`updateAll` writes back to `t_visit` (both the result columns and `c_ta_update`).
`visit_invalidate_trigger` fires only on `INSERT`/`DELETE` of `t_visit`
(narrowed from `INSERT OR UPDATE OR DELETE` in V1210), so these `UPDATE`s do not
re-invalidate obscalc.

## What a recompute actually computes

`update(visitId)` rebuilds the visit's `TimeAccountingState` from **all** of its
events, then applies discounts and corrections to produce the invoice:

- **No-data discount** — visit has no datasets → whole charge discounted.
- **Daylight discount** — time outside the night's nautical twilight.
- **Overlap discount** — the portion overlapping another chargeable visit at the
  same site.
- **QA discount** — atoms whose datasets failed QA.
- **Corrections** — staff `Add`/`Subtract` entries from `t_time_charge_correction`
  are reapplied on top (so corrections survive a recompute; only the base they
  apply to reflects the current algorithm).

Final charge = raw execution time − discounts, then corrected.

## Known limitation: cross-visit overlap

The overlap discount makes one visit's charge depend on *another* visit. Per-visit
scoping only recomputes the visit that received an event/QA change, so if an
earlier visit is **open but no longer receiving events** while a new visit
overlaps it, the earlier visit will not be re-marked and could miss an overlap
adjustment. This is accepted: overlapping visits realistically both receive
events while concurrent (so both get marked and recomputed), and the alternative
— whole-observation recompute — reintroduces the corrected-history problem above,
which is worse.

## Readers

The stored `t_visit` values are read by GraphQL query paths only (never in a
write/execution path), so eventual consistency is safe:

- `Visit.timeChargeInvoice` — `TimeChargeInvoiceMapping` over `VisitTable`
  (`executionTime`/`finalCharge` are the stored `c_raw_*`/`c_final_*` columns;
  discounts/corrections from their tables). **Stored, not recomputed on read.**
- `Observation.execution` time charge — `ExecutionMapping` via
  `timeAccountingService.selectObservations` (sums the visits).
- `Program` banded time — `ProgramMapping` via `selectProgram`.

obscalc itself does **not** read time accounting, so there is no dependency
cycle in that direction.
