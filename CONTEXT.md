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
The single sky position an Archive Duplication Search is run around: the observation's resolved base position (explicit base if set, otherwise the asterism center at the reference time). Non-sidereal targets are searched by target name instead, via `GoaParams.NonSidereal`.

**Search Radius**:
Half the observation's field of view, derived per observing mode from lucuma-core's science-area geometry.

**Archive Vocabulary**:
The terms GOA uses in its own records — instrument names, observation classes, QA states — drawn from the OCS era and open-ended. A superset of GPP's vocabulary: the archive holds data from instruments, classes and states GPP has no case for, and it does not normalize casing. Always preserved verbatim as the record of what the archive actually said.
_Avoid_: raw value, GOA enum (the archive's terms are not an enumeration we control).

**Vocabulary Projection**:
The best-effort reading of an Archive Vocabulary term in GPP's own vocabulary, exposed beside it as a typed field. Partial by design: it asserts a GPP term only where that term means the same thing the archive meant, and is absent otherwise — so `partnerCal` has no `ObserveClass` (GPP dropped the partner charge class) and a failing QA state has no `DatasetQaState` (the search asks GOA for non-failing records, so it is out of contract). Derived when read rather than stored, so a frozen snapshot benefits when a projection improves. The Archive Vocabulary term remains the system of record.
_Avoid_: parsing, normalization, conversion (all imply the projection is total).

**Submission Freeze**:
The rule that an observation's Archive Duplication Search snapshot becomes read-only once its proposal is submitted, so the Match Count seen by the TAC and the PDF is exactly what the PI last saw. Before submission every refresh overwrites the snapshot; after submission refresh is rejected.

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
The uploaded file describing a Custom Mask's actual slitlet layout, an attachment of type `mos_mask`. Optional and absent by default: the mask is usually only designed during Phase 2, from pre-imaging. Its absence is the normal early state of a valid MOS observation, not an error. Carries a Mask Name.
_Avoid_: mask file, MDF, mask id.

**Mask Name**:
The observatory's identifier for the physical plate a Mask Attachment describes, such as `GN2025AQ001-01` — what is written on the plate, what the mask cutting queue calls it, and what eventually reaches the instrument. Names the *plate*, where the Mask Attachment names the *file* and the Custom Mask names the *FPU*. Always present on a Mask Attachment and unique within a program, but never entered: it is derived from the attachment and is read-only everywhere it appears. The ODB is not the system of record for its syntax, so no convention is enforced on it.
_Avoid_: MDF name, mask file name, mask id.

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
A telluric a PI has explicitly declined, held inactive so it will not be observed even when its science observation is active. Reversible — reinstating the telluric resumes its science observation's lifecycle.
_Avoid_: disabled telluric, skipped telluric, deleted telluric, opted-out telluric.
