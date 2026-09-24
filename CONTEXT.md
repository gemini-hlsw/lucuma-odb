# lucuma-odb

The project glossary. One repo-wide set of terms, grouped by the area they belong to.

## Language

### Archive Duplication Search

**Gemini Observatory Archive (GOA)**:
The external Gemini science archive at `archive.gemini.edu`, queried to warn a PI/TAC that a proposed observation may already have been taken. The ODB reaches it through the `GoaClient` from lucuma-core.
_Avoid_: "the archive" (ambiguous), Gemini Science Archive.

**Archive Duplication Search**:
The per-observation check that queries GOA around the observation's pointing for existing observations of the same field and instrument. Advisory only — it never blocks creation or submission.
_Avoid_: duplicate detection, dedup, conflict check.

**GOA Match**:
One record returned by an Archive Duplication Search — a single archived file, decoded as a `GoaSummaryRecord`. A match is file-level, not observation-level.
_Avoid_: hit, result, duplicate.

**Match Count**:
The number of GOA Matches for an observation, counted at file level to stay consistent with the PIT. This is the number persisted and shown to the TAC / carried to the proposal PDF.
_Avoid_: number of duplicates, observation count.

**Saturated**:
The state of a Match Count that hit GOA's hard 500-record-per-query cap. A saturated count is rendered as "500+"; the true count is unknown and deliberately not pursued.

**Duplication-Equivalence Group**:
An ODB-owned grouping of instruments treated as interchangeable for duplication purposes, so a proposed observation is searched against every archive instrument in its group. GMOS-N/GMOS-S map to the single `GMOS` archive umbrella; Alopeke/Zorro and F2/GNIRS are each searched as a pair. Distinct from lucuma-core's 1:1 `goaName`.
_Avoid_: instrument alias, overlapping instruments.

**Mode Class**:
The coarse imaging-vs-spectroscopy classification of an observing mode, where spectroscopy is the umbrella over longslit, MOS, and IFS and speckle (Alopeke/Zorro) classifies as imaging. Carried by `lucuma.core.enums.ScienceMode`, whose tags are the GOA query tokens (`imaging`, `spectroscopy`). The Archive Duplication Search restricts matches to the proposed observation's own Mode Class by sending that token alongside the instrument family; a generic visitor instrument has no Mode Class and sends no token.
_Avoid_: observing mode (the ODB's full mode is finer), instrument mode.

**Search Center**:
The single sky position an Archive Duplication Search is run around: the observation's explicit base if set, otherwise the asterism center at the reference time. A wholly non-sidereal asterism is searched by target name instead, via `GoaParams.NonSidereal`. An asterism mixing sidereal and non-sidereal targets has no usable Search Center — a cone around the composite center would miss the moving member's archive history — so without an explicit base the search declines and reports Not Applicable rather than a false all-clear.

**Search Radius**:
Half the observation's field of view, derived per observing mode from lucuma-core's science-area geometry.

**Archive Vocabulary**:
The terms GOA uses in its own records — instrument names, observation classes, QA states — drawn from the OCS era and open-ended. A superset of GPP's vocabulary: the archive holds data from instruments, classes and states GPP has no case for, and it does not normalize casing. Always preserved verbatim as the record of what the archive actually said.
_Avoid_: raw value, GOA enum (the archive's terms are not an enumeration we control).

**Vocabulary Projection**:
The best-effort reading of an Archive Vocabulary term in GPP's own vocabulary, exposed beside it as a typed field. Partial by design: it asserts a GPP term only where that term means the same thing the archive meant, and is absent otherwise — so `partnerCal` has no `ObserveClass` (GPP dropped the partner charge class) and a failing QA state has no `DatasetQaState` (the search asks GOA for non-failing records, so it is out of contract). Derived when read rather than stored, so a frozen snapshot benefits when a projection improves. The Archive Vocabulary term remains the system of record.
_Avoid_: parsing, normalization, conversion (all imply the projection is total).

**Submission Freeze**:
The rule that an observation's Archive Duplication Search snapshot is read-only from submission until acceptance, so the Match Count seen by the TAC and the PDF is exactly what the PI last saw. A proposal that is not accepted stays frozen. Acceptance lifts the freeze, so newly requested targets in an accepted program can be checked; before submission and after acceptance every refresh overwrites the snapshot.

**Completion Freeze**:
The rule that a completed observation's Archive Duplication Search snapshot is read-only, whatever the proposal status. Its own data is now in GOA, so a re-check could only find itself. Completion is either declared by the PI or reached by execution, as the materialized workflow state reports it. Unlike submission, a completion that lands while a search is in flight is not serialised against the snapshot write; the window is short and the search is advisory.

**Stale Snapshot**:
A stored Archive Duplication Search snapshot whose provenance no longer matches the observation: the GOA query URLs the search policy would build today differ from the ones recorded with the snapshot. Materialized as `t_obscalc.c_archive_stale` by the background obscalc calculation and exposed as `stale` in GraphQL. Never true for an observation that was never searched, that has nothing searchable now, or that is under the Submission Freeze or the Completion Freeze. Staleness looks only at the observation side; GOA gaining new files does not stale a snapshot.
_Avoid_: outdated, dirty, invalid (the snapshot remains valid evidence of what was asked).

### Focal Plane Units

**FPU (Focal Plane Unit)**:
Whatever sits at the instrument's focal plane and defines the aperture light passes through. Unqualified, the term is ambiguous — always say Builtin FPU or Custom Mask, because an observing mode carries exactly one of the two and they are different kinds of thing.

**Builtin FPU**:
A fixed aperture permanently available in the instrument, named by an enumerated value (`GmosNorthFpu.LongSlit_1_00`). Long slit and IFU modes carry one. A MOS mode never does.
_Avoid_: FPU (ambiguous), slit, aperture.

**Custom Mask**:
The FPU of a MOS observation: a physically machined plate carrying many slitlets, one per target. Modelled as a required Custom Slit Width plus an optional Mask Attachment. It occupies the slot a Builtin FPU would, and the two are mutually exclusive.
_Avoid_: MDF (that names the file, not the FPU), mask FPU, custom FPU.

**Custom Slit Width**:
The representative width of the slitlets cut into a Custom Mask, from `GmosCustomSlitWidth`. Always known — it is how the mode is chosen at Phase 0 — and so is required even when the mask itself has not been made. In 1:1 correspondence with the builtin long slit FPU widths, which is what lets a MOS observation be calibrated as a long slit.

**Mask Attachment**:
The uploaded file describing a Custom Mask's actual slitlet layout, an attachment of type `mos_mask`. Optional and absent by default: the mask is usually only designed during Phase 2, from pre-imaging. Its absence is the normal early state of a valid MOS observation, not an error. Carries a Mask Name and a Mask Instrument, and may only be assigned to a MOS observation whose instrument matches it.
_Avoid_: mask file, MDF, mask id.

**Mask Name**:
The observatory's identifier for the physical plate a Mask Attachment describes, such as `GN2025AQ001-01` — what is written on the plate, what the mask cutting queue calls it, and what eventually reaches the instrument. Names the *plate*, where the Mask Attachment names the *file* and the Custom Mask names the *FPU*. Always present on a Mask Attachment and unique within a program, but never entered: it is derived from the attachment and is read-only everywhere it appears. The ODB is not the system of record for its syntax, so no convention is enforced on it.
_Avoid_: MDF name, mask file name, mask id.

### Mask Designs

**Mask Definition**:
The design parsed out of a Mask Attachment's file at upload and recorded on the attachment: Mask Instrument, pixel scale, pointing, position angle, dispersion direction and the slit list. Names the *design*, where the Mask Attachment names the file, the Mask Name the plate and the Custom Mask the FPU. Derived data — the uploaded file remains the source of truth and the definition can always be rebuilt from it. A file that cannot be parsed, or that records no position angle, rejects the upload, so a definition exists exactly for masks accepted since parsing began.
_Avoid_: mask metadata, mask blob (names the storage, not the concept), ODF (names the file format). Also beware the unrelated `lucuma.core.model.MaskDefinition`, which is the "assigned or not yet assigned" state of a Custom Mask's Mask Attachment, not a design at all.

**Mask Instrument**:
The instrument a Mask Attachment's plate was cut for — one of GMOS North, GMOS South or Flamingos-2, the three that do MOS at Gemini. Read from the mask file, so always part of the Mask Definition and never entered. The two GMOS arms are distinct answers, not one, because a plate is machined for one arm and cannot be mounted in the other. Always present on a Mask Attachment: a design that names anything else is refused on upload.
_Avoid_: mask site (the site does not distinguish GMOS from Flamingos-2), mask type.

**Alignment Box**:
An aperture in a Mask Definition cut for a bright star and used to position the mask on sky during acquisition, not to take a spectrum. Distinguished from science slits by its ACQUISITION placement priority — never by shape or slit type, which do not discriminate.
_Avoid_: acquisition star (names the star, not the aperture), alignment slit.

**Science Slit**:
Any aperture in a Mask Definition that is not an Alignment Box — the slits that place science objects. The population behind the science slit count and the Average Slit Width.

**Average Slit Width**:
The mean width of a Mask Definition's Science Slits. Alignment Boxes are excluded so their wide apertures do not skew it; a design with no Science Slits has no average.

**Dispersion Direction**:
The axis along which the instrument spreads a spectrum, in pre-image detector coordinates — horizontal for GMOS, vertical for Flamingos-2. It decides how a mask file's x and y columns are read: a slit's *width* is always its extent along the dispersion direction, its *length* the extent across it.

**Slit Offsets and Tilt**:
The signed quantities on a slit: displacement along the slit's length, displacement across its width (which drives a point source off the slit), and the tilt, bounded to ±45°. Held in the wrapping `Angle` type but always rendered in the signed representation, the way Declination is — −1.5″ reads as −1.5″, never as a full turn less. Reading them as plain magnitudes is a bug.
_Avoid_: unsigned or wrapped readings of these fields.

### Observing Modes

**MOS (Multi-Object Spectroscopy)**:
A spectroscopy observing mode that observes many targets at once through a Custom Mask. Classifies as spectroscopy for Mode Class purposes. Distinct from long slit only in its focal plane — it shares long slit's grating, filter, central wavelength and readout, and is calibrated as a long slit.
_Avoid_: multislit and multiple slit (both name the *focal plane*, not the mode), MOS mode.

**Pre-Imaging**:
Imaging taken so a Custom Mask can be designed from it. A property of the *imaging* observation that produces the frames, never of the MOS observation that later consumes the mask.
_Avoid_: MOS imaging, mask imaging.

### ITC Results

**Peak Pixel Flux**:
The highest electron count in any single pixel of any CCD in an ITC calculation, used to judge how close an exposure comes to saturating the detector. One number per target per configuration, taken as the maximum across the CCDs the ITC reports. Absent whenever no CCD data reached the ODB.
_Avoid_: peak counts, peak e- count, ADU (a gain-scaled view of the same quantity), percent full well (a well-depth-scaled view).

### Tellurics

**Telluric**:
A calibration observation created automatically alongside a science spectroscopic observation, observing a standard star used to correct for atmospheric absorption in the science data. By default it follows its science observation's lifecycle state.
_Avoid_: telluric standard (names the target star, not the observation).

**Declined Telluric**:
A telluric a PI has explicitly declined, held inactive so it will not be observed even when its science observation is active. Reversible — reinstating the telluric resumes its science observation's lifecycle. Distinct from a Telluric Type of NoTelluric, which is declarative and prevents generation up front: the type governs whether tellurics exist, the decline governs whether an existing one is observed. The two are uncoupled — setting NoTelluric deletes tellurics (unless they have visits or execution), taking any decline state with them; restoring a real type never reinstates a declined telluric.
_Avoid_: disabled telluric, skipped telluric, deleted telluric, opted-out telluric.

**Calibration Epoch**:
A stretch of roughly 90 minutes of science time (the Calibration Epoch Interval) after which an observation's night-time calibrations are assumed to need repeating: a fresh SmartGCal flat and arc set and, unless the Telluric Type is `NoTelluric`, a telluric. An estimating unit, not a scheduling one — epochs do not line up with visits (a visit can hold more or less than an epoch), so the Calibration Count and the setup count disagree by design.
_Avoid_: visit (a scheduling unit), calibration set, telluric interval.

**Multi-Telluric Threshold**:
The science time per visit (90 minutes today) above which a visit is given two tellurics, one before and one after the science, instead of one after. A scheduling rule applied by the calibrations service when tellurics are materialised; it shares its value with the Calibration Epoch Interval by coincidence, and the two are separate constants so they may diverge.
_Avoid_: telluric threshold, 1.5 h rule, epoch interval (the estimating constant).

**Calibration Count**:
The number of calibration epochs an observation is expected to need over its remaining science time, roughly one per 90 minutes, carried in its time estimate. Each epoch is expected to bring a SmartGCal flat and arc set and, unless the Telluric Type is `NoTelluric`, a telluric. Zero for calibration observations themselves and for modes that take no telluric. Independent of the Telluric Type, which decides what an epoch costs, not how many there are. Distinct from the number of tellurics materialised per visit (one, or two above the multi-telluric threshold), which is a scheduling rule, not an estimate. It does not yet feed the time estimate.
_Avoid_: telluric count (the concept is broader than tellurics), number of calibrations.

**Telluric Type**:
*What kind* of standard star a telluric should observe (`Hot`, `A0V`, `Solar`, `Manual`), or `NoTelluric` for no telluric at all. A property of the observing-mode config. Setting `NoTelluric` overwrites the previous choice (a `Manual` star list is not remembered across it), and changing the observing mode resets the type to its default (`Hot`) — a "no tellurics" decision for one mode must not be assumed to carry to another.
_Avoid_: telluric (names the observation, not the classification), requires-telluric flag (superseded design).

**Derived Telluric S/N**:
The signal-to-noise a Telluric's science exposure time mode is defaulted to: twice its science observation's, uncapped, so the standard is measured at least as deeply as the target. Where the science's own S/N comes from depends on how the PI expressed it — a requested signal-to-noise is taken at face value, while a Time & Count science has no requested S/N, so the ITC's achieved total signal-to-noise is used instead. Read from the science's live configurations only. Where a mode carries an exposure time mode per central wavelength, the Telluric has one configuration per *distinct* science wavelength, sized from the deepest science configuration at that wavelength, with the averaged reference wavelength and the largest coadds of the group; a daytime pinhole keeps the science list row for row. Always system-owned: it is written as derived, never as a PI's choice, and is recomputed whenever its science observation's ITC result changes.
_Avoid_: telluric signal-to-noise (ambiguous — the ITC also computes one *for* the telluric), doubled S/N, telluric S/N target.

### Time Estimates

**Step Digest**:
For one kind of step within a sequence (bias, dark, arc, flat or observing): how many there are and how much time they take, split by charge class. Step time includes the configuration change that precedes the step, so the science-fold move into GCAL is charged to the arc that follows it. A sequence carries its five step digests together as `steps`, which partition it so their times always sum to its time estimate. Steps are bucketed by step type; a GCAL step whose lamp is not an arc counts as a flat. Stored per sequence as count, non-charged time and program time columns in `t_obscalc` and `t_execution_digest`; digests recorded before the breakdown are backfilled with everything as observing time and then recomputed. No generator emits biases or darks yet, so those buckets are zero.
_Avoid_: GCAL digest (the buckets are not only GCAL steps), calibration digest (calibrations also means telluric observations), flat/arc time (loses the count).

**GCAL Set**:
An atom that contains at least one GCAL step, counted per sequence as `gcalSets`. GMOS puts an arc and a flat in every science atom, so it has one set per atom; Flamingos-2 and GNIRS put theirs in a single calibration atom, which may hold only a flat or only an arc. Not deducible from the arc and flat step counts, since a set need not contain one of each. It is what a Calibration Epoch is expected to bring, so the cost of one set and the sets left in a sequence both come from it.
_Avoid_: calibration set (calibrations also means telluric observations), flat/arc pair (a set may hold only one).

**Observing Time**:
The step digest of everything that is not a bias, dark, arc or flat: science exposures, acquisition and offsets, including the science-fold move back to sky. Excludes setup time.
_Avoid_: science time (the sequence's full time estimate, which includes GCAL steps), on-source time (observing time also contains offsets), exposure time.

### AEON / Multi-Facility Proposals

**AEON Multi-Facility Proposal**:
A Gemini proposal (Queue, Classical, or Large Program) that is part of the AEON/multi-facility program, meaning the project also requests time at non-Gemini facilities. Membership is carried by the presence of the proposal's `aeonMultiFacility` object rather than by a boolean, so the AEON Required Instruments have nowhere to live unless the proposal is in the program.
_Avoid_: AEON proposal (AEON is the network, not the proposal), MF proposal, multi-facility flag.

**AEON Required Instrument**:
An instrument an AEON Multi-Facility Proposal declares indispensable: the project is infeasible without its requested Gemini time. Scheduling information only. Expressed as a set — an instrument is required by being in the set, and every other instrument is not required, which is the default. Site is never stated; it follows from the instrument.
_Avoid_: required configuration ("configuration" means something else in the ODB), required time flag.

**Backing Observation**:
An observation that makes an instrument eligible to be an AEON Required Instrument: it is present (not deleted), active (not user-deactivated), and its observing mode maps to that instrument. An instrument may only be marked required while it has a Backing Observation, and the mark is removed the moment its last Backing Observation goes away — by deletion, deactivation, or a mode change. Taking the proposal out of the program (nulling `aeonMultiFacility`, or switching proposal type) likewise clears the whole set.

### Program Status

**Program Status**:
The effective status of a program: the Explicit Status when one is declared, otherwise the Default Status. One of Active, Inactive, Complete, Incomplete. Advisory — the ODB reports it (and the scheduler filters on it) but does not gate observation workflow with it.
_Avoid_: program state, activation.

**Default Status**:
The derived layer of Program Status: Active when the current UTC date falls within the Active Period (inclusive of both bounds), Inactive otherwise. Time-varying; never Complete or Incomplete.
_Avoid_: computed status, isActive (removed from the API; query the effective status instead).

**Explicit Status**:
A staff-declared status that masks the Default Status; any status may be declared, including Active (forcing a program active outside its Active Period). Clearing it returns the program to its derived status. Setting and clearing are both staff-only; reading is not.
_Avoid_: status override, manual status.

**Active Period**:
The `[activeStart, activeEnd]` date interval during which a program's observations may be scheduled, defaulted from the Call for Proposals when there is one. Staff-editable. The source of the Default Status.
_Avoid_: observing window (that is an observation-level concept), semester dates.

### Observation Priority

**Observation Priority**:
The PI's declared statement of which of their observations matter more, one of Low, Medium or High, carried by `lucuma.core.enums.ObservationPriority`. Always present — every observation has one, defaulting to Medium, so there is no "unset" to distinguish from a deliberate Medium. Advisory: the scheduler may weigh it when choosing among a program's observations, and nothing in the ODB reads it. A single shared value, not one per role: staff edit the same field the PI does, and a staff edit replaces the PI's statement outright.

### Science Band

**Science Band**:
The ranking band (Band 1 through Band 4) an observation is scheduled under, drawn from the bands its program was allocated time in. Optional on an observation, but the scheduler cannot place an observation without one. A program with no allocations cannot carry a band at all.
_Avoid_: band (alone, ambiguous with photometric bands), ranking, priority (that is Observation Priority).

**Band Requirement**:
The rule that an observation in an allocated program is not Defined, and so cannot be Ready, until it has a Science Band. It applies to calibration observations too, as the one validation they are subject to, so a calibration that inherits no band from its science observations is Undefined. Programs with no allocations are exempt.
_Avoid_: band check, band gate.

**Inherited Band**:
The Science Band a calibration observation takes from the science observations it serves: the best band among matching observations for a per-program calibration, a copy of the parent's for a per-science one. Best-effort, which is why the Band Requirement still checks the calibration itself.
