# Resource Service — Initial Domain Model

## Core

The calendar domain uses `TimestampInterval` from **lucuma-core** as the
canonical representation of time ranges. It defines an interval
`[start, end)`.

Source:
https://github.com/gemini-hlsw/lucuma-core/blob/main/modules/core/shared/src/main/scala/lucuma/core/util/TimestampInterval.scala

```mermaid
classDiagram
    class TimestampInterval {
        Timestamp start
        Timestamp end
    }

    class TooSupport {
        <<enum>>
        STANDARD
        INTERRUPT
        RAPID
        NONE
    }

    class Usage {
        <<enum>>
        SCIENCE
        ENGINEERING
    }
```

## Telescope

These types describe telescope-level calendar entries.
Each entry applies to a specific site (GN/GS) and a specific time range.

```mermaid
classDiagram

    %% Telescope operational availability.
    class TelescopeAvailabilityStatus {
        TimestampInterval interval
        TelescopeAvailability availability
        String reason?
        TelescopeAvailability plannedAvailability?
        Site site
    }

    class TelescopeAvailability {
        <<enum>>
        OPEN
        CLOSED
    }

    %% Telescope ToO acceptance for an interval.
    class TelescopeTooStatus {
        TimestampInterval interval
        TooSupport tooSupport
        Site site
    }

    %% Telescope observing mode for an interval.
    class TelescopeModeStatus {
        TimestampInterval interval
        Site site
        TelescopeMode mode
    }

    %% Base telescope mode type.
    class TelescopeMode { }

    %% Mode variants.
    class QueueMode { }
    class ClassicalMode { ProgramReference program }
    class PriorityVisitorMode { ProgramReference program }
    class EngineeringMode { }
    class CommissioningMode { }

    TelescopeModeStatus --> TelescopeMode
    TelescopeMode <|-- QueueMode
    TelescopeMode <|-- ClassicalMode
    TelescopeMode <|-- PriorityVisitorMode
    TelescopeMode <|-- EngineeringMode
    TelescopeMode <|-- CommissioningMode
```

## Instruments

These types describe instrument availability, usage mode, and ToO capability over time.

```mermaid
classDiagram

    %% Instrument operational status for an interval.
    class InstrumentStatus {
        Boolean available
        Usage usage
        %% TODO: Is this time-dependent as well?
        TooSupport tooSupport
        InstrumentLocation location
        TimestampInterval interval
        Instrument instrument
        Site site
    }

    %% Base instrument location type.
    class InstrumentLocation { }

    %% Location variants.
    class PortLocation { Int portNumber }
    class FloorLocation { }
    class LabLocation { }
    class BaseLocation { }
    class UnknownLocation { }
    class OtherLocation { String description }

    InstrumentLocation <|-- PortLocation
    InstrumentLocation <|-- FloorLocation
    InstrumentLocation <|-- LabLocation
    InstrumentLocation <|-- BaseLocation
    InstrumentLocation <|-- UnknownLocation
    InstrumentLocation <|-- OtherLocation
```

## Instrument Components Status and Call for Proposals Component Availability

Component operational status over time.
These types describe instrument component definitions.
GMOS components shown here; other instruments may extend InstrumentComponent similarly.
Proposal availability does not necessarily match operational availability for components.

```mermaid
classDiagram

    %% Component operational status.
    class InstrumentComponentStatus {
        Boolean available
        Usage usage
        TimestampInterval interval
        InstrumentComponent component
    }

    %% Base instrument component type.
    class InstrumentComponent {
        Site site
    }

    %% Base GMOS component grouping.
    class GmosComponent { }
    class GmosNorthComponent { }
    class GmosSouthComponent { }

    InstrumentComponent <|-- GmosComponent
    GmosComponent <|-- GmosNorthComponent
    GmosComponent <|-- GmosSouthComponent

    %% Shared GMOS component types.
    class GmosNodAndShuffle { Boolean supported }

    GmosComponent <|-- GmosNodAndShuffle

    %% GMOS North component types.
    class GmosNorthFilter {
        GmosNorthFilter filter
    }
    class GmosNorthGrating {
        GmosNorthGrating grating
    }
    class GmosNorthFpu {
        GmosNorthBuiltinFpu fpu
    }
    class GmosNorthOiwfs {}

    GmosNorthComponent <|-- GmosNorthFilter
    GmosNorthComponent <|-- GmosNorthGrating
    GmosNorthComponent <|-- GmosNorthFpu
    GmosNorthComponent <|-- GmosNorthOiwfs

    %% GMOS South component types.
    class GmosSouthFilter {
        GmosSouthFilter filter
         }
    class GmosSouthGrating {
        GmosSouthGrating grating
        }
    class GmosSouthFpu {
        GmosSouthBuiltinFpu fpu
        }
    class GmosSouthOiwfs {}

    GmosSouthComponent <|-- GmosSouthFilter
    GmosSouthComponent <|-- GmosSouthGrating
    GmosSouthComponent <|-- GmosSouthFpu
    GmosSouthComponent <|-- GmosSouthOiwfs

    %% Time-windowed inclusion of a component in proposal calls.
    class InstrumentComponentCallForProposalsAvailability {
        TimestampInterval interval
        InstrumentComponent component
        CallForProposalsType? callForProposalsType
        List~string~? callForProposalsIds
    }
```
