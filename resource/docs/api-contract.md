# Resource API Contract

RUI = Resource User Interface

This document describes the GraphQL queries and mutations used by RUI to manage Resource catalog definitions and Resource blocks.

Resource stores catalog definitions separately from time-bounded Resource blocks.

Catalog definitions describe what exists. Resource blocks describe what is available, where, and when.

## Conventions

Catalog mutations return the created or updated record.

Delete mutations set `existence = DELETED`. They do not hard-delete records.

Deleted definitions are hidden from normal RUI creation flows but remain valid for existing historical blocks.

## Catalogs

Catalog queries provide the values RUI needs for selection controls and management screens.

Catalog records use a stable `id`. Most catalog records also have a `code`.

- `id` is the stable opaque Resource identifier.
- `code` is the machine-friendly technical identifier shown in RUI and used in imports, exports, search, and diagnostics.

For instruments, telescope subsystems, and instrument components, users usually create and maintain the `code` as part of the catalog record. Examples include `GMOS_NORTH`, `PWFS1`, and `R_PRIME`.

Enum fields are used for constrained behavior, grouping, validation, or scheduler semantics. Examples include `InstrumentType`, `SubsystemType`, `ComponentType`, `Availability`, and `TooSupport`.

### Instruments

```graphql
type Instrument {
  id: InstrumentId!
  code: String!
  name: String!
  instrumentType: InstrumentType!
  existence: Existence!
  description: String
}

enum InstrumentType {
  FACILITY
  VISITING
  FUTURE
  RETIRED
  OTHER
}

instruments(
  existence: Existence = PRESENT
  instrumentType: InstrumentType
  code: String
): [Instrument!]!

instrument(id: InstrumentId!): Instrument
```

```graphql
createInstrument(input: CreateInstrumentInput!): Instrument!
updateInstrument(input: UpdateInstrumentInput!): Instrument!
deleteInstrument(id: InstrumentId!): Instrument!

input CreateInstrumentInput {
  code: String!
  name: String!
  instrumentType: InstrumentType!
  description: String
}

input UpdateInstrumentInput {
  id: InstrumentId!
  code: String
  name: String
  instrumentType: InstrumentType
  existence: Existence
  description: String
}
```

### Telescope Subsystems

```graphql
type TelescopeSubsystem {
  id: TelescopeSubsystemId!
  code: String!
  name: String!
  site: Site!
  subsystemType: SubsystemType!
  existence: Existence!
  description: String
}

enum SubsystemType {
  WAVEFRONT_SENSOR
  ADAPTIVE_OPTICS
  LASER_GUIDE_STAR
  DOME
  POWER
  POLARIMETER
  OTHER
}

telescopeSubsystems(
  site: Site
  existence: Existence = PRESENT
  subsystemType: SubsystemType
  code: String
): [TelescopeSubsystem!]!

telescopeSubsystem(id: TelescopeSubsystemId!): TelescopeSubsystem
```

```graphql
createTelescopeSubsystem(input: CreateTelescopeSubsystemInput!): TelescopeSubsystem!
updateTelescopeSubsystem(input: UpdateTelescopeSubsystemInput!): TelescopeSubsystem!
deleteTelescopeSubsystem(id: TelescopeSubsystemId!): TelescopeSubsystem!

input CreateTelescopeSubsystemInput {
  code: String!
  name: String!
  site: Site!
  subsystemType: SubsystemType!
  description: String
}

input UpdateTelescopeSubsystemInput {
  id: TelescopeSubsystemId!
  code: String
  name: String
  site: Site
  subsystemType: SubsystemType
  existence: Existence
  description: String
}
```

`TelescopeSubsystem` is site-specific. Subsystem codes should be unique within a site.

### Instrument Components

```graphql
type InstrumentComponent {
  id: InstrumentComponentId!
  code: String!
  name: String!
  instrument: Instrument!
  componentType: ComponentType!
  existence: Existence!
  description: String
}

enum ComponentType {
  FILTER
  GRATING
  FPU
  OIWFS
  NOD_AND_SHUFFLE
}

instrumentComponents(
  existence: Existence = PRESENT
  instrumentId: InstrumentId
  instrumentCode: String
  componentType: ComponentType
  code: String
): [InstrumentComponent!]!

instrumentComponent(id: InstrumentComponentId!): InstrumentComponent
```

```graphql
createInstrumentComponent(input: CreateInstrumentComponentInput!): InstrumentComponent!
updateInstrumentComponent(input: UpdateInstrumentComponentInput!): InstrumentComponent!
deleteInstrumentComponent(id: InstrumentComponentId!): InstrumentComponent!

input CreateInstrumentComponentInput {
  instrumentId: InstrumentId!
  code: String!
  name: String!
  componentType: ComponentType!
  description: String
}

input UpdateInstrumentComponentInput {
  id: InstrumentComponentId!
  instrumentId: InstrumentId
  code: String
  name: String
  componentType: ComponentType
  existence: Existence
  description: String
}
```

Component codes should be unique within an instrument and component type.

## RUI View Queries

RUI primarily uses view-specific queries. Each query returns the data required for that view, shaped for rendering.

```graphql
nightTimeline(site: Site!, observingNight: Date!): NightTimeline!

weekTimeline(site: Site!, weekStart: Date!): WeekTimeline!

monthCalendar(site: Site!, yearMonth: YearMonth!): MonthCalendar!

semesterCalendar(site: Site!, semester: Semester!): SemesterCalendar!
```

The amount of detail returned varies by view:

- `nightTimeline` returns the complete set of Resource blocks needed to render the nightly timeline.
- `weekTimeline` returns the Resource blocks needed for a week timeline.
- `monthCalendar` returns summarized day-level information for a monthly calendar.
- `semesterCalendar` returns high-level summaries for long-term planning.

These queries are optimized for RUI views rather than exposing the storage model directly.

## Resource Blocks

Resource block queries expose the underlying Resource blocks directly. They are used by editors, detail panels, and administrative workflows.

All block queries are scoped by `site` and `interval`.

### Telescope Blocks

```graphql
telescopeAvailabilityBlocks(
  site: Site!
  interval: TimestampIntervalInput!
): [TelescopeAvailabilityBlock!]!

telescopeModeBlocks(
  site: Site!
  interval: TimestampIntervalInput!
): [TelescopeModeBlock!]!

telescopeTooSupportBlocks(
  site: Site!
  interval: TimestampIntervalInput!
): [TelescopeTooSupportBlock!]!
```

```graphql
createTelescopeAvailabilityBlock(input: CreateTelescopeAvailabilityBlockInput!): TelescopeAvailabilityBlock!
updateTelescopeAvailabilityBlock(input: UpdateTelescopeAvailabilityBlockInput!): TelescopeAvailabilityBlock!
deleteTelescopeAvailabilityBlock(id: TelescopeAvailabilityBlockId!): TelescopeAvailabilityBlock!

createTelescopeTooSupportBlock(input: CreateTelescopeTooSupportBlockInput!): TelescopeTooSupportBlock!
updateTelescopeTooSupportBlock(input: UpdateTelescopeTooSupportBlockInput!): TelescopeTooSupportBlock!
deleteTelescopeTooSupportBlock(id: TelescopeTooSupportBlockId!): TelescopeTooSupportBlock!

createTelescopeModeBlock(input: CreateTelescopeModeBlockInput!): TelescopeModeBlock!
updateTelescopeModeBlock(input: UpdateTelescopeModeBlockInput!): TelescopeModeBlock!
deleteTelescopeModeBlock(id: TelescopeModeBlockId!): TelescopeModeBlock!
```

### Telescope Subsystem Blocks

```graphql
telescopeSubsystemBlocks(
  site: Site!
  interval: TimestampIntervalInput!
  subsystemId: TelescopeSubsystemId
  subsystemType: SubsystemType
  code: String
): [TelescopeSubsystemBlock!]!
```

```graphql
createTelescopeSubsystemBlock(input: CreateTelescopeSubsystemBlockInput!): TelescopeSubsystemBlock!
updateTelescopeSubsystemBlock(input: UpdateTelescopeSubsystemBlockInput!): TelescopeSubsystemBlock!
deleteTelescopeSubsystemBlock(id: TelescopeSubsystemBlockId!): TelescopeSubsystemBlock!

input CreateTelescopeSubsystemBlockInput {
  site: Site!
  interval: TimestampIntervalInput!
  subsystemId: TelescopeSubsystemId!
  availability: Availability!
  reason: String
  restriction: TelescopeSubsystemRestrictionInput
}

input UpdateTelescopeSubsystemBlockInput {
  id: TelescopeSubsystemBlockId!
  site: Site
  interval: TimestampIntervalInput
  subsystemId: TelescopeSubsystemId
  availability: Availability
  reason: String
  restriction: TelescopeSubsystemRestrictionInput
}
```

`TelescopeSubsystemBlock.site` must match the referenced `TelescopeSubsystem.site`.

### Instrument Availability Blocks

```graphql
instrumentAvailabilityBlocks(
  site: Site!
  interval: TimestampIntervalInput!
  instrumentId: InstrumentId
  instrumentCode: String
  instrumentType: InstrumentType
): [InstrumentAvailabilityBlock!]!
```

```graphql
createInstrumentAvailabilityBlock(input: CreateInstrumentAvailabilityBlockInput!): InstrumentAvailabilityBlock!
updateInstrumentAvailabilityBlock(input: UpdateInstrumentAvailabilityBlockInput!): InstrumentAvailabilityBlock!
deleteInstrumentAvailabilityBlock(id: InstrumentAvailabilityBlockId!): InstrumentAvailabilityBlock!

input CreateInstrumentAvailabilityBlockInput {
  site: Site!
  interval: TimestampIntervalInput!
  instrumentId: InstrumentId!
  availability: Availability!
  location: InstrumentLocationInput!
  tooSupport: TooSupport!
  reason: String
}

input UpdateInstrumentAvailabilityBlockInput {
  id: InstrumentAvailabilityBlockId!
  site: Site
  interval: TimestampIntervalInput
  instrumentId: InstrumentId
  availability: Availability
  location: InstrumentLocationInput
  tooSupport: TooSupport
  reason: String
}
```

`Instrument` does not store `site`. The site is represented by `InstrumentAvailabilityBlock.site`.

### Instrument Component Availability Blocks

```graphql
instrumentComponentAvailabilityBlocks(
  site: Site!
  interval: TimestampIntervalInput!
  instrumentId: InstrumentId
  instrumentCode: String
  componentId: InstrumentComponentId
  componentType: ComponentType
  componentCode: String
): [InstrumentComponentAvailabilityBlock!]!
```

```graphql
createInstrumentComponentAvailabilityBlock(input: CreateInstrumentComponentAvailabilityBlockInput!): InstrumentComponentAvailabilityBlock!
updateInstrumentComponentAvailabilityBlock(input: UpdateInstrumentComponentAvailabilityBlockInput!): InstrumentComponentAvailabilityBlock!
deleteInstrumentComponentAvailabilityBlock(id: InstrumentComponentAvailabilityBlockId!): InstrumentComponentAvailabilityBlock!

input CreateInstrumentComponentAvailabilityBlockInput {
  site: Site!
  interval: TimestampIntervalInput!
  componentId: InstrumentComponentId!
  availability: Availability!
  reason: String
}

input UpdateInstrumentComponentAvailabilityBlockInput {
  id: InstrumentComponentAvailabilityBlockId!
  site: Site
  interval: TimestampIntervalInput
  componentId: InstrumentComponentId
  availability: Availability
  reason: String
}
```

For scheduler-facing use, component availability only contributes to schedulability when the component's owning instrument has overlapping availability at the same site.

## Scheduler Queries

Scheduler queries expose a scheduler-facing availability view for a site and interval.

The scheduler prefers one query that returns the Resource data needed for scheduling. The real-time scheduler is expected to request one observing-night-sized interval. Simulation mode may request a larger interval, up to a semester.

Resource does not define observing-night boundaries in this API. The caller provides the interval. Resource returns scheduler-relevant resources that are available during at least part of that interval.

Returned intervals should be clipped to the requested interval.

```graphql
schedulerResources(
  site: Site!
  interval: TimestampIntervalInput!
): SchedulerResources!
```

`schedulerResources` returns a scheduler-facing projection of Resource data. It does not return every stored Resource block.

Unavailable resources are omitted from the scheduler response. Reasons for unavailability remain available through Resource block queries for RUI, diagnostics, and reporting.

Presence in the scheduler response determines schedulability:

```text
If the required resource is present for the interval, it may be scheduled.
If the required resource is absent for the interval, it should not be scheduled.
```

`SCIENCE` and `ENGINEERING` availability may both be returned. `UNAVAILABLE` resources are omitted.

```text
SCIENCE -> available for science scheduling
ENGINEERING -> available for engineering scheduling
absent -> unavailable for scheduling
```

Components are returned under their parent instrument. A component is included only when both the component and parent instrument are available for the same interval.

The response includes:

- telescope availability
- telescope observing modes
- telescope ToO support
- telescope subsystem availability and restrictions
- instrument availability
- instrument location
- instrument ToO support
- instrument component availability, including filters and gratings

Resource is not the source of weather or fault details. If weather or a fault affects scheduling, Resource should expose the scheduling impact by omitting the unavailable resource from `schedulerResources`. Fault and weather details may come from JIRA, Chronicle, the weather service, or another system.

Raw Resource block queries remain available for RUI editing, detail panels, and diagnostics.

### Response Shape

```graphql
type SchedulerResources {
  site: Site!
  interval: TimestampInterval!
  telescopeAvailability: [SchedulerTelescopeAvailabilityBlock!]!
  telescopeModes: [TelescopeModeBlock!]!
  tooSupport: [TelescopeTooSupportBlock!]!
  subsystemBlocks: [SchedulerTelescopeSubsystemBlock!]!
  instrumentBlocks: [SchedulerInstrumentBlock!]!
}

type SchedulerTelescopeAvailabilityBlock {
  interval: TimestampInterval!
  availability: TelescopeAvailability!
}

type SchedulerTelescopeSubsystemBlock {
  interval: TimestampInterval!
  subsystem: TelescopeSubsystem!
  availability: Availability!
  restriction: TelescopeSubsystemRestriction
}

type SchedulerInstrumentBlock {
  interval: TimestampInterval!
  instrument: Instrument!
  availability: Availability!
  location: InstrumentLocation!
  tooSupport: TooSupport!
  componentBlocks: [SchedulerInstrumentComponentBlock!]!
}

type SchedulerInstrumentComponentBlock {
  interval: TimestampInterval!
  component: InstrumentComponent!
  availability: Availability!
}
```

### Example Query

```graphql
query SchedulerResources {
  schedulerResources(
    site: GN
    interval: {
      start: "2026-08-01T22:00:00Z"
      end: "2026-08-02T22:00:00Z"
    }
  ) {
    site
    interval {
      start
      end
    }
    telescopeAvailability {
      interval {
        start
        end
      }
      availability
    }
    telescopeModes {
      interval {
        start
        end
      }
      mode {
        __typename
        ... on QueueMode {
          id
        }
        ... on ClassicalMode {
          programs
        }
        ... on EngineeringMode {
          id
        }
      }
    }
    tooSupport {
      interval {
        start
        end
      }
      tooSupport
    }
    subsystemBlocks {
      interval {
        start
        end
      }
      subsystem {
        id
        code
        name
        subsystemType
      }
      availability
      restriction {
        __typename
        ... on ElevationRestriction {
          minDegrees
          maxDegrees
        }
        ... on PointingRestriction {
          description
        }
        ... on PowerSourceRestriction {
          currentSource
        }
      }
    }
    instrumentBlocks {
      interval {
        start
        end
      }
      instrument {
        id
        code
        name
        instrumentType
      }
      availability
      location {
        __typename
        ... on PortLocation {
          portNumber
        }
        ... on FloorLocation {
          description
        }
        ... on LabLocation {
          description
        }
        ... on BaseLocation {
          description
        }
        ... on UnknownLocation {
          description
        }
        ... on OtherLocation {
          description
        }
      }
      tooSupport
      componentBlocks {
        interval {
          start
          end
        }
        component {
          id
          code
          name
          componentType
        }
        availability
      }
    }
  }
}
```

### Example Response

```json
{
  "data": {
    "schedulerResources": {
      "site": "GN",
      "interval": {
        "start": "2026-08-01T22:00:00Z",
        "end": "2026-08-02T22:00:00Z"
      },
      "telescopeAvailability": [
        {
          "interval": {
            "start": "2026-08-01T22:00:00Z",
            "end": "2026-08-02T22:00:00Z"
          },
          "availability": "OPEN"
        }
      ],
      "telescopeModes": [
        {
          "interval": {
            "start": "2026-08-01T22:00:00Z",
            "end": "2026-08-02T10:00:00Z"
          },
          "mode": {
            "__typename": "QueueMode",
            "id": "queue"
          }
        },
        {
          "interval": {
            "start": "2026-08-02T10:00:00Z",
            "end": "2026-08-02T22:00:00Z"
          },
          "mode": {
            "__typename": "EngineeringMode",
            "id": "engineering"
          }
        }
      ],
      "tooSupport": [
        {
          "interval": {
            "start": "2026-08-01T22:00:00Z",
            "end": "2026-08-02T10:00:00Z"
          },
          "tooSupport": "INTERRUPT"
        },
        {
          "interval": {
            "start": "2026-08-02T10:00:00Z",
            "end": "2026-08-02T22:00:00Z"
          },
          "tooSupport": "NONE"
        }
      ],
      "subsystemBlocks": [
        {
          "interval": {
            "start": "2026-08-01T22:00:00Z",
            "end": "2026-08-02T22:00:00Z"
          },
          "subsystem": {
            "id": "ts-pwfs1",
            "code": "PWFS1",
            "name": "PWFS1",
            "subsystemType": "WAVEFRONT_SENSOR"
          },
          "availability": "SCIENCE",
          "restriction": null
        }
      ],
      "instrumentBlocks": [
        {
          "interval": {
            "start": "2026-08-01T22:00:00Z",
            "end": "2026-08-02T10:00:00Z"
          },
          "instrument": {
            "id": "i-gmos-n",
            "code": "GMOS_NORTH",
            "name": "GMOS-N",
            "instrumentType": "FACILITY"
          },
          "availability": "SCIENCE",
          "location": {
            "__typename": "PortLocation",
            "portNumber": 5
          },
          "tooSupport": "INTERRUPT",
          "componentBlocks": [
            {
              "interval": {
                "start": "2026-08-01T22:00:00Z",
                "end": "2026-08-02T10:00:00Z"
              },
              "component": {
                "id": "ic-r-prime",
                "code": "R_PRIME",
                "name": "r′",
                "componentType": "FILTER"
              },
              "availability": "SCIENCE"
            },
            {
              "interval": {
                "start": "2026-08-01T22:00:00Z",
                "end": "2026-08-02T10:00:00Z"
              },
              "component": {
                "id": "ic-g-prime",
                "code": "G_PRIME",
                "name": "g′",
                "componentType": "FILTER"
              },
              "availability": "SCIENCE"
            },
            {
              "interval": {
                "start": "2026-08-01T22:00:00Z",
                "end": "2026-08-02T10:00:00Z"
              },
              "component": {
                "id": "ic-b1200",
                "code": "B1200_G5301",
                "name": "B1200",
                "componentType": "GRATING"
              },
              "availability": "SCIENCE"
            }
          ]
        },
        {
          "interval": {
            "start": "2026-08-02T10:00:00Z",
            "end": "2026-08-02T22:00:00Z"
          },
          "instrument": {
            "id": "i-gmos-n",
            "code": "GMOS_NORTH",
            "name": "GMOS-N",
            "instrumentType": "FACILITY"
          },
          "availability": "ENGINEERING",
          "location": {
            "__typename": "PortLocation",
            "portNumber": 5
          },
          "tooSupport": "NONE",
          "componentBlocks": [
            {
              "interval": {
                "start": "2026-08-02T10:00:00Z",
                "end": "2026-08-02T22:00:00Z"
              },
              "component": {
                "id": "ic-b1200",
                "code": "B1200_G5301",
                "name": "B1200",
                "componentType": "GRATING"
              },
              "availability": "ENGINEERING"
            }
          ]
        }
      ]
    }
  }
}
```