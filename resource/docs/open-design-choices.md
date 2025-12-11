# Resource Design Choices

This document tracks the main Resource v1 design choices that still need agreement.

## 1. GraphQL Schema Complexity

We should decide where GraphQL unions/interfaces are worth the added client complexity.

Current concern:

```text
InstrumentLocation
TelescopeSubsystemRestriction
TelescopeMode
```

`TelescopeMode` probably deserves a union because each mode has different scheduler meaning.

`InstrumentLocation` may be better as a flat object:

```graphql
type InstrumentLocation {
  locationType: InstrumentLocationType!
  portNumber: Int
  description: String
}
```

## 2. Draft and Published Schedules

Resource needs a way to save work before it becomes user-facing.

## 3. Catalog Management UI

RUI needs screens to manage Resource catalog records:

```text
Instruments
Telescope subsystems
Instrument components
```

Each catalog page should support:

```text
Search
Filter
Create
Edit
Soft delete
Restore if needed
```

Suggested UI: simple PrimeReact table plus detail/edit dialog or side panel.

Key questions:

```text
Who can edit catalog records?
Can codes be changed after creation?
```

## 4. Scheduler Response Shape

Scheduler wants one query:

```graphql
schedulerResources(site, interval)
```

Current direction:

```text
Return only resources available for scheduling
Omit unavailable resources
Include intervals because availability may change within the request
Return components under their parent instrument
```

Presence means schedulable:

```text
Present with SCIENCE -> usable for science scheduling
Present with ENGINEERING -> usable for engineering scheduling
Absent -> not schedulable
```

## 5. Component Availability

Components can be stored independently, but scheduler queries should only return components when the parent instrument is also available for the same interval.

Open questions:

```text
Should RUI warn when component availability does not overlap instrument availability?
Do components need location in v1?
```

## 7. Availability Editing

RUI should edit operational blocks through a site-scoped schedule view.

```text
GN view -> blocks where site = GN
GS view -> blocks where site = GS
```

Needed actions:

```text
Create block
Edit block
Delete block
Copy block
Split block
Adjust interval
```

Open questions:

```text
Do telescope availability/mode/ToO need complete coverage?
```

## 8. Validation and Permissions

Baseline validation:

```text
interval.start < interval.end
Instrument code is globally unique
Subsystem code is unique within site
Component code is unique within instrument and component type
TelescopeSubsystemBlock.site matches TelescopeSubsystem.site
New blocks reference PRESENT catalog records
```