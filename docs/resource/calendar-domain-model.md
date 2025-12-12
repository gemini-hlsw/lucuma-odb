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

    class Instrument {
        <<enum>>
        GMOS_NORTH
        GMOS_SOUTH
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
        Instrument instrument
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

## Telescope Subsystem

Telescope subsystem status over time.

```mermaid
classDiagram

    %% Coarse discriminator
    class Subsystem {
        <<enum>>
        PWFS1
        PWFS2
        GEMS
        ALTAIR
        LGS
        GPOL
        DOME_SHUTTERS
        DOME_VENT_GATES
        POWER_SOURCE
    }

    %% ADT root (not instantiable)
    class TelescopeSubsystem {
        Subsystem subsystem
    }

    %% Operational status over time (site-specific)
    class TelescopeSubsystemStatus {
        Boolean available
        Usage usage
        TimestampInterval interval
        Site site
    }

    %% One subsystem, many time-bounded statuses
    TelescopeSubsystemStatus --> TelescopeSubsystem

    %% Simple subsystems
    TelescopeSubsystem <|-- Pwfs1
    TelescopeSubsystem <|-- Pwfs2
    TelescopeSubsystem <|-- Gpol
    TelescopeSubsystem <|-- Gems
    TelescopeSubsystem <|-- Altair
    TelescopeSubsystem <|-- Lgs

    %% Subsystems with additional domain data
    TelescopeSubsystem <|-- DomeShutters
    class DomeShutters {
        DomeShutterElevationRange allowedElevation
    }

    TelescopeSubsystem <|-- DomeVentGates
    class DomeVentGates {
        %% TODO: Very generic right now.
        PointingRestriction pointingRestriction
    }

    TelescopeSubsystem <|-- PowerSourceSubsystem
    class PowerSourceSubsystem {
        PowerSourceSettings currentSource
    }

    %% Supporting enums
    class PowerSourceSettings {
        <<enum>>
        COMMERCIAL
        GENERATOR
    }

    class PointingRestriction {
        <<enum>>
        NONE
        LIMITED
        PROHIBITED
    }

    class DomeShutterElevationRange {
        Float minDegrees
        Float maxDegrees
    }
```

## Staff

Staff observer qualifications and instrument training certifications status.

```mermaid
classDiagram

    class StaffMember {
        String id
        String firstName
        String lastName
    }

    %% TODO: Does staff need to be qualifed per site?
    class ObserverQualification {
        Site site
        TimestampInterval certifiedPeriod
    }

    class InstrumentTraining {
        Instrument instrument
        Site site
        TimestampInterval certifiedPeriod
    }

    StaffMember "1" --> "0..*" ObserverQualification
    StaffMember "1" --> "0..*" InstrumentTraining

```
