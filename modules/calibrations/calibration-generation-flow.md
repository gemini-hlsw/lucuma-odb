# Calibration Generation Flow

## Overview

The ODB automatically generates calibration observations for science programs. When a science observation's configuration changes, a background daemon detects the change and triggers calibration recalculation. There are two distinct strategies depending on the instrument and calibration type.

## Trigger Chain

All calibration generation begins with a database change detected via PostgreSQL NOTIFY/LISTEN channels.

```mermaid
sequenceDiagram
    participant User
    participant GraphQL as GraphQL Mutation
    participant DB as PostgreSQL
    participant Trigger as DB Trigger
    participant Daemon as Calibrations Daemon
    participant Service as CalibrationsService

    User->>GraphQL: updateObservations / updateAsterisms / updateTargets
    GraphQL->>DB: UPDATE t_observation / t_asterism_target / t_target
    DB->>Trigger: invalidate_obscalc()
    Trigger->>DB: UPDATE t_obscalc SET state = 'pending'
    Note over DB: obscalc recalculates asynchronously
    DB->>Trigger: ch_obscalc_update_trigger
    Trigger-->>Daemon: NOTIFY ch_obscalc_update (oid, pid, old_obscalc_state, new_obscalc_state, old_workflow_state, new_workflow_state, TG_OP)
    Daemon->>Daemon: Filter: obscalc state transitioned to Ready (oldState != newState), not a calibration
    Daemon->>Service: recalculateCalibrations(pid, referenceInstant, oid)
```

## Entry Point: `recalculateCalibrations`

`CalibrationsService.scala:136`

This method orchestrates both calibration strategies. It loads all observations for the program, splits them by instrument type, and delegates to the appropriate service.

```mermaid
flowchart TD
    A[recalculateCalibrations] --> B[Fetch calibration targets from DB]
    A --> C[Load all science observations]
    A --> D[Load all calibration observations]
    D --> E[Exclude Ongoing/Completed calibrations]

    C --> F{Split by instrument}
    F -->|F2 / IGRINS2 Long-Slit| G[perObs: map to CalibrationConfigSubset]
    F -->|GMOS N/S Long-Slit| H[perProgram: keep as ObservingMode]

    G --> I[PerScienceObservationCalibrationsService.generateCalibrations]
    H --> J[PerProgramPerConfigCalibrationsService.generateCalibrations]
    E --> J

    I --> K[Collect added/removed observation IDs]
    J --> K
    K --> L[Delete orphaned calibration targets]
    L --> M[Return added, removed]
```

## Strategy 1: Per-Science-Observation Calibrations (F2 / IGRINS2 Telluric)

`PerScienceObservationCalibrationsService.scala`

Each Flamingos2 or IGRINS2 science observation gets its own telluric calibration observations. The number of tellurics depends on the science observation's execution duration. Matching between instruments is handled by `CalibrationConfigMatcher` (`Flamingos2LS`, `Igrins2LS`) and `ObsExtract.perObsFilter` accepts both `Flamingos2Config` and `Igrins2Config`.

### Flow

```mermaid
flowchart TD
    A[generateCalibrations pid, scienceObs, oid] --> B[Find the changed observation in scienceObs]
    B --> C[Filter to Defined/Ready workflow states only]
    C --> D[Set all constraints to deferred]
    D --> E[Load program group tree]

    E --> F{Is observation still active?}
    F -->|Yes: Defined/Ready| G[generateTelluricForScience]
    F -->|No: other state| H[cleanupOrphanedTelluricGroup]

    G --> I[Find or create telluric group for this science obs]
    I --> J[syncTelluricObservation]

    J --> K[Find existing telluric obs in group]
    K --> L[Filter out non-deletable obs]
    L --> M[Query science observation duration]

    M --> N{Duration?}
    N -->|> 1.5 hours| O[Need 2 tellurics: Before + After]
    N -->|<= 1.5 hours| P[Need 1 telluric: After]
    N -->|No duration| Q[Need 0 tellurics]

    O --> R{Existing count matches needed count?}
    P --> R
    Q --> R

    R -->|No| S[Delete all deletable tellurics]
    S --> T[Create new telluric observations]
    R -->|Yes| U[Sync configuration on existing tellurics]

    T --> V[For each telluric:]
    V --> W[Clone observing mode from science obs]
    W --> X[Set telluric exposure time mode]
    X --> Y["S/N = min(science_S/N x 2, 100)"]
```

### Telluric Observation Details

- Telluric observations are placed in a **group with the science observation**
- The observing mode is **cloned from the science observation** with adjusted exposure parameters
- Groups are ordered: `[Telluric Before, Science, Telluric After]` or `[Science, Telluric After]`
- Telluric observations are created with `calibrationRole = Telluric`; their workflow validation state resolves directly to `Defined` (calibrations skip the regular validation path in `ObservationWorkflowService`)

### Telluric Workflow State Mirroring

The telluric's effective workflow state is **inherited from its parent science observation** rather than stored on the telluric itself:

- `ObservationValidationInfo.effectiveUserState` returns `associatedUserState` when `role === Telluric` (`ObservationWorkflowService.scala:142`); that value is pulled via a `LEFT JOIN t_observation s` on the same `c_group_id` with `c_calibration_role IS NULL` (`ObservationWorkflowService.scala:810`).
- While the telluric's state is `<= Ready`, `allowedTransitions` is forced to `Nil` (line 467). Direct calls to `setWorkflowState` on a telluric are rejected with `InvalidWorkflowTransition` (`graphql/mapping/AccessControl.scala:739`).
- Setting the science obs to `Ready` promotes the telluric from `Defined` to `Ready`; setting science to `Inactive` flips the telluric to `Inactive` (line 460 ensures `Inactive` overrides `Ongoing`).

### Telluric Target Resolution

After a telluric observation is created, `TelluricTargetsService` asynchronously resolves its target star from HIP catalog data. Brightness limits (`c_hmin_hot`, `c_hmin_solar`) are loaded at startup into an `HminBrightnessCache` keyed by `HminBrightnessKey.F2(disperser, filter, fpu)` or `HminBrightnessKey.Igrins2`. For F2 the key comes from the instrument config; IGRINS2 has a single entry because it has no per-config variation.

### Deletion Protection

All calibration deletion guards in `CalibrationsUtils.scala` read execution state from `v_generator_params.c_execution_state` (an `ExecutionState`) rather than `t_obscalc.c_workflow_state`. `v_generator_params` is derived directly from `t_execution_event` / `t_step_execution`, so it flips to `ongoing` the moment an event lands — no obscalc recalc required. This applies to:

- `excludeOngoingAndCompleted` — base helper used by the GMOS deletion paths (`CalibrationsService.recalculateCalibrations`, `PerProgramPerConfigCalibrationsService.removeUnnecessaryCalibrations`).
- `excludeFromDeletion` — composes the above with a `t_visit`-based check.
- `excludeTelluricsFromDeletion` — also checks the **parent science observation** in the same group, for the sc-8614 case where the parent has started executing but obscalc hasn't refreshed.

For tellurics specifically, even if the telluric's own mirrored state still says `Defined`/`Ready`, the telluric is protected when the parent science obs has started executing (`Ongoing`, `Completed`, or `DeclaredComplete`).

```mermaid
flowchart TD
    A[Can this telluric be deleted?] --> B{Telluric workflow state?}
    B -->|Ongoing| C[NO - protected]
    B -->|Completed| C
    B -->|Defined/Ready| D{Has visits?}
    D -->|Yes| C
    D -->|No| F{Parent science execution state?}
    F -->|Ongoing| C
    F -->|Completed| C
    F -->|DeclaredComplete| C
    F -->|NotStarted / NotDefined| E[YES - safe to delete]
```

The parent state is resolved by `selectTelluricScienceExecutionStates`, which joins `t_observation` to itself by `c_group_id` (picking the row with `c_calibration_role IS NULL`) and then to `v_generator_params` for the execution state. Both `syncTelluricObservation` and `cleanupOrphanedTelluricGroup` use this filter.

## Strategy 2: Per-Program-Per-Config Calibrations (GMOS SpectroPhotometric + Twilight)

`PerProgramPerConfigCalibrationsService.scala`

For GMOS North and South Long-Slit, the system creates **one calibration observation per unique instrument configuration per calibration role** across the entire program. Multiple science observations sharing the same config share a single calibration.

### Flow

```mermaid
flowchart TD
    A["generateCalibrations(pid, allSci, allCalibs, calibTargets, when)"] --> B[Filter existing calibrations to SpectroPhoto/Twilight roles]
    A --> C[Filter science obs to Defined/Ready only]

    C --> D[Extract unique GMOS configurations from science obs]
    D --> E[Prepare ideal targets for GN and GS sites]
    D --> D2["calObsProps: per role, group all science obs by normalized config -> avg wavelength + band"]

    E --> F[calculateConfigurationsPerRole]
    F --> G["For each role (SpectroPhoto, Twilight):"]
    G --> H[Normalize science configs for this role]
    G --> I[Normalize existing calibration configs for this role]
    H --> J["Diff: needed = scienceConfigs - calibConfigs"]
    I --> J

    J --> K{Any configs missing calibrations?}
    K -->|Yes| L[generateGMOSLSCalibrations for missing configs]
    K -->|No| M[Skip creation]

    B --> N[removeUnnecessaryCalibrations]
    N --> O[For each existing calibration:]
    O --> P{Is it needed by any science obs?}
    P -->|No| Q[Delete if not Ongoing/Completed]
    P -->|Yes| R[Keep]

    L --> S["Update band and wavelength on existing calibrations (lookup by the calibration's own role-normalized config)"]
    M --> S
    S --> T[Delete empty calibration groups]
```

`calObsProps` keys props by each science observation's **role-normalized** config — the same normalization applied to a calibration's stored config
at creation (see Configuration Matching below) — producing a `Map[CalibrationConfigSubset, CalObsProps]` per role. Both creation and the
existing-calibration update look up by the calibration's own role, so the key always matches. Keying by the raw config instead would silently
miss any calibration whose normalized fields differ — e.g. a specphot calibration's ROI is normalized to `CentralSpectrum`, so a `FullFrame`
science obs would never match it, leaving its S/N λ stale.

### Configuration Matching

The matching logic differs by calibration role:

```mermaid
flowchart TD
    A[CalibrationConfigMatcher] --> B{Calibration Role?}

    B -->|SpectroPhotometric| C[SpecphotoGmosLS]
    C --> D["Normalize: set ROI = CentralSpectrum"]
    D --> E["Compare: normalize(config1) === normalize(config2)"]
    E --> F["ROI differences are ignored"]

    B -->|Twilight| G[TwilightGmosLS]
    G --> H["No normalization"]
    H --> I["Compare: config1 === config2"]
    I --> J["All config details must match exactly"]
```

### Calibration Observation Creation

```mermaid
sequenceDiagram
    participant Service as PerProgramPerConfigService
    participant Target as TargetService
    participant Obs as ObservationService
    participant Group as GroupService

    Service->>Service: For each missing config + role + site
    Service->>Target: cloneTargetInto(idealTargetId, programId)
    Target-->>Service: cloned Target.Id

    alt SpectroPhotometric
        Service->>Obs: Create obs with S/N = 100, wavelength, science band
        Note over Obs: Position angle = AverageParallactic
        Note over Obs: Constraints = SpecPhotoCalibration
    else Twilight
        Service->>Obs: Create obs with S/N = 100, central wavelength
        Note over Obs: Position angle = Fixed
        Note over Obs: Constraints = TwilightCalibration
    end

    Obs-->>Service: new Observation.Id
    Service->>Service: Set calibration role on observation
    Service->>Group: Add to "Calibrations" group
```

### Target Selection

Calibration targets are pre-defined in the database under calibration programs. The system selects the best target for a given site and role using `CalibrationIdealTargets`:

```mermaid
flowchart TD
    A[CalibrationIdealTargets] --> B[Query t_target joined with t_program]
    B --> C["Filter: program has calibration role, existence = present"]
    C --> D[Compute coordinates at reference instant using SiderealTracking]
    D --> E{Calibration Role?}
    E -->|SpectroPhotometric| F[bestSpecPhotoTarget for site]
    E -->|Twilight| G[bestTwilightTarget for site]
    F --> H[Return Target.Id]
    G --> H
```

## Calibration Target Recalculation

A separate flow handles updating a calibration's target when its observation time changes.

```mermaid
sequenceDiagram
    participant DB as PostgreSQL
    participant Daemon as Calibrations Daemon
    participant Service as CalibrationsService
    participant Target as TargetService
    participant Asterism as AsterismService

    Note over DB: UPDATE t_observation SET c_observation_time (on a calibration)
    DB-->>Daemon: NOTIFY ch_calib_obs_time (pid, oid, 'UPDATE')
    Daemon->>Service: recalculateCalibrationTarget(pid, oid)

    Service->>DB: Query calibration time, role, and observing mode type
    Service->>Asterism: Get current asterism target IDs

    alt GMOS North or South Long-Slit
        Service->>DB: Query all calibration targets
        Service->>Service: Compute coordinates at observation time
        Service->>Service: Select best target for site + role
        Service->>Target: cloneTargetInto(newTargetId, pid)
        Service->>Asterism: updateAsterism(add new target, remove old targets)
    end

    Service->>Target: deleteOrphanCalibrationTargets(pid)
```

## Workflow State Guards Summary

| Operation | Allowed States | Blocked States |
|-----------|---------------|----------------|
| Process science observation | Defined, Ready | All others |
| Modify calibration observation | execution state NotStarted/NotDefined | Ongoing, Completed, DeclaredComplete |
| Delete calibration observation | execution state NotStarted/NotDefined (no visits) | Ongoing, Completed, DeclaredComplete, or has visits |
| Delete telluric calibration | Above, **and** parent science obs execution state NotStarted/NotDefined | Same as above, plus parent science Ongoing/Completed/DeclaredComplete |
| Directly transition a telluric via `setWorkflowState` | None (while ≤ Ready) | All transitions rejected; state mirrors science obs |
