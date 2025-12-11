# Scheduler Resource Mapping

This document describes how Resource data will be exposed to the scheduler.

Resource stores time-bounded Resource blocks. The scheduler is expected to consume a scheduler-facing view over a requested timestamp interval.

The exact response shape is intentionally left open until the scheduler requirements are better understood.

## Initial Scheduler Query

Proposed:

```text
schedulerResources(site, interval)
```

The scheduler should be able to request the Resource information required for scheduling over a timestamp interval.

## Resource Data

Resource v1 currently models:

- Telescope availability
- Telescope observing modes
- Telescope ToO support
- Telescope subsystem availability and restrictions
- Instrument availability
- Instrument component availability

These models should provide the information needed to construct the scheduler view. The final response shape will be driven by scheduler requirements.

## Scheduler Availability Response

Based on scheduler team feedback, the v1 scheduler response returns resources that are available for scheduling, not every stored Resource block.

The intended scheduler behavior is:

```text
If the required resource is present in the response for the interval, it may be scheduled.
If it is absent, it should not be scheduled.
```

This is a scheduler-facing projection of Resource data. It does not expose every unavailable block.

Intervals are still included because telescope modes, ToO support, instruments, and components may change within the requested interval.

Components are returned under their parent instrument. A component is included only when both the component and parent instrument are available for the same interval.

## Input from Scheduler Team

Scheduler team feedback guiding this design:

- Prefer one query rather than separate resource queries.
- Real-time scheduling primarily requests an observing night.
- Simulation may request a larger interval, up to a semester.
- The scheduler wants resources that are available for the requested interval.
- The scheduler should be able to check whether a resource is present in the response and schedule with it if present.
- Instrument configuration availability, such as filters and gratings, is required.
- Faults and weather should generally come from their owning services.