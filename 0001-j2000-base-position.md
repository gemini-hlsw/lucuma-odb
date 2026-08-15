# Materialize an observation J2000 Base Position for cone-search filters

The `observations` query needs a `targetCoordinates` cone filter like the one
`configurationRequests` gained in SC-9240. That filter was only pushable to SQL
because configuration requests carry frozen, indexed reference coordinates; an
observation has nothing comparable — its base position is computed on demand
(asterism composite, proper-motion corrected) by the tracking service, so there
is no column a candidate query can scan.

We decided to materialize a **J2000 Base Position** per observation: the
explicit base if set, otherwise the asterism's composite center with every
target proper-motion corrected to the fixed epoch J2000.0. Only observations
whose asterism is entirely sidereal (or which have an explicit base) get one;
anything containing a non-sidereal or opportunity target has no position and is
invisible to coordinate filters — and, having no position, it *does* match a
negated cone. The value is computed during the obscalc pass and stored with its
results, riding the existing target/asterism/observation invalidation triggers;
existing rows are backfilled by a migration that marks obscalc `pending`
(the V1220/V1254 pattern). It is deliberately filter-only: the live
`basePosition` output field remains the only public position.

## Why a fixed epoch, and why J2000.0

A position corrected to "now" goes stale by the mere passage of time, forcing a
periodic recompute of every row; a fixed epoch changes only when the target or
asterism changes, so trigger-driven invalidation suffices. J2000.0 is the
standard reference epoch users already assume when typing coordinates. The
drift cost is negligible for this use — typical proper motion is under 0.1″/yr,
a couple of arcseconds over decades, far below plausible cone radii — with the
documented caveat that extreme-PM stars (Barnard's star class, ~10″/yr) can sit
outside a small cone centered on their present-day position.

## Considered options

- **CfP midpoint, like configuration requests.** Rejected: different programs
  would get different instants (and unsubmitted programs have no CfP), defeating
  the point of comparing positions at one uniform timestamp.
- **Compute in SQL by joining target tables.** Rejected: multi-target composite
  centers and rigorous PM correction (lucuma-core's 3D math with radial
  velocity/parallax) are impractical in SQL, and indexes don't help through the
  join.
- **Compute per candidate in Scala at query time.** Rejected: an unindexable
  full scan of effectful per-observation computation.
- **Synchronous maintenance in the mutation services.** Rejected: every code
  path touching targets, asterisms, or the explicit base would need to
  recompute in-transaction — many scattered call sites, easy to miss one.
  The obscalc route trades that for eventual consistency: the stored position
  can briefly lag an edit.

## Consequences

- The filter reflects obscalc's view, not the transactional one: an observation
  edited moments ago may match (or miss) a cone until the daemon catches up,
  and observations obscalc has never processed have no position.
- The deploy that introduces the column pays one full obscalc sweep (including
  ITC lookups) to backfill.
- A future request for epoch-accurate matching (query-time PM correction) is a
  new feature, not a tweak — the stored position is fixed-epoch by design.
