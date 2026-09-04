// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*

/**
 * The renderer input, built when a job is taken and handed to pyexplore as a
 * JSON file.  This is the contract with `pdf/payload.py`
 * (`generate_from_payload`):
 *
 *  - `schemaVersion`: refused by the renderer on a major-version mismatch.
 *  - `program`: the `query program` response as the pdf scripts receive it
 *    from the ODB, i.e. `{"data": {"program": ...}}`; `parse_response.parse`
 *    consumes it unchanged and ignores fields it does not know.
 *  - `attachments`: presigned GET URLs for the SCIENCE and TEAM PDFs.
 *  - `observations`: the science observations that go in the tables, with the
 *    ITC inputs snap needs.  Inactive and undefined observations and
 *    calibrations are left out here; Python drops them again, since it also
 *    serves the standalone scripts that query the ODB directly.
 *
 * The json shared by both repositories is pyexplore's
 * `pdf/tests/fixtures/payload-v1.json`.
 */
object PdfSummaryJobPayload:

  val SchemaVersion: String = "1.0.0"

  // Observations per proposal the payload can carry; a larger one fails the job
  // rather than rendering a truncated summary.
  val MaxObservations: Int = 1000

  case class AttachmentUrl(fileName: String, url: String) derives Encoder.AsObject

  def build(program: Json, observations: List[Json], attachments: List[AttachmentUrl]): Json =
    Json.obj(
      "schemaVersion" -> SchemaVersion.asJson,
      "program"       -> Json.obj("data" -> Json.obj("program" -> program)),
      "observations"  -> observations.asJson,
      "attachments"   -> attachments.asJson
    )

  /**
   * One operation with two root selections.  The `program` selection is the
   * pdf scripts' `program.graphql`; the `observations` selection is the union
   * of snap's `Observations` (table) and `Observation` (ITC input) queries.
   * They are kept as separate roots on purpose: snap forwards `sourceProfile`
   * and `exposureTimeMode` straight to the ITC, so merging the two selections
   * (which would add a second unit to some quantities) would break ITC input.
   *
   * A plain string rather than a clue operation: the query runs in-process
   * through `runGraphQLQuery`, its result is forwarded to Python as raw JSON, and
   * the shape test executes it against the live schema on every build, which is
   * what compile-time validation would have added.
   */
  val Query: String =
    s"""
query PdfSummaryJobPayload($$programId: ProgramId!) {
  program(programId: $$programId) {
    id
    name
    description
    active {
      start
      end
    }
    pi {
      ...ProgramUser
    }
    users {
      ...ProgramUser
    }
    timeEstimateRange {
      value {
        maximum { total { hours } }
        minimum { total { hours } }
      }
    }
    attachments {
      fileName
      id
      description
      attachmentType
      fileSize
    }
    proposal { ...Proposal }
    groupElements {
      observation { id }
      group {
        ...Group
        elements {
          observation { id }
          group {
            ...Group
            elements {
              observation { id }
              group {
                ...Group
                elements {
                  observation { id }
                  group {
                    ...Group
                    elements {
                      observation { id }
                      group {
                        ...Group
                        elements {
                          observation { id }
                          group {
                            ...Group
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    observations {
      hasMore
      matches { ...Observation }
    }
  }
  observations(
    WHERE: {
      program: { id: { EQ: $$programId } }
      workflow: { workflowState: { NIN: [INACTIVE, UNDEFINED] } }
      calibrationRole: { IS_NULL: true }
    }
    LIMIT: ${MaxObservations}
  ) {
    hasMore
    matches { ...ItcObservation }
  }
}

fragment Proposal on Proposal {
  reference { label }
  call { semester }
  category
  gemini {
    scienceSubtype

    ... on Classical {
      minPercentTime
      partnerSplits {
        partner
        percent
      }
      exchangePartner
      aeonMultiFacility { requiredInstruments }
      jwstSynergy
      usLongTerm
    }

    ... on DemoScience {
      tooActivationCeiling
      minPercentTime
    }

    ... on DirectorsTime {
      tooActivationCeiling
      minPercentTime
    }

    ... on FastTurnaround {
      tooActivationCeiling
      minPercentTime
    }

    ... on LargeProgram {
      tooActivationCeiling
      minPercentTime
      totalTime { hours }
      aeonMultiFacility { requiredInstruments }
      jwstSynergy
    }

    ... on PoorWeather {
      __typename
    }

    ... on Queue {
      tooActivationCeiling
      minPercentTime
      partnerSplits {
        partner
        percent
      }
      exchangePartner
      considerForBand3
      aeonMultiFacility { requiredInstruments }
      jwstSynergy
      usLongTerm
    }

    ... on SystemVerification {
      tooActivationCeiling
      minPercentTime
    }
  }

  keck {
    minPercentTime
    partnerSplits {
      partner
      percent
    }
  }

  subaru {
    minPercentTime
    partnerSplits {
      partner
      percent
    }
  }
}

fragment ProgramUser on ProgramUser {
  thesis
  displayName
  affiliation
  email
  educationalStatus
}

fragment Group on Group {
  id
  name
  parentId
  description
  calibrationRoles
  parentIndex
  existence
  system
  minimumRequired
  minimumInterval { hours }
  maximumInterval { hours }
  timeEstimateRange {
    calculationState
    state
    value {
      maximum {
        program { hours }
        total { hours }
      }
      minimum {
        program { hours }
        total { hours }
      }
    }
  }
  ordered
  sameNight
}

fragment Observation on Observation {
  id
  title
  targetEnvironment { firstScienceTarget { ...Target } }
  constraintSet { ...ConstraintSet }
  schedulingConstraints { ...SchedulingConstraints }
  observingMode { ...ObservingMode }
  execution { ...Execution }
  groupId
  calibrationRole
  workflow { value { state } }
  archiveDuplication { state matchCount lastCheckedAt }
}

fragment ConstraintSet on ConstraintSet {
  imageQuality
  cloudExtinction
  skyBackground
  waterVapor
  elevationRange {
    airMass {
      max
      min
    }
    hourAngle {
      maxHours
      minHours
    }
  }
}

fragment SchedulingConstraints on SchedulingConstraints {
  tooActivation
  schedulingMode
  timingWindows {
    startUtc
    inclusion
    end {
      ... on TimingWindowEndAt { atUtc }
      ... on TimingWindowEndAfter {
        after { hours }
        repeat {
          times
          period { hours }
        }
      }
    }
  }
}

fragment ObservingMode on ObservingMode {
  instrument
  mode
  exchange {
    mode
    keckInstrument
    subaruInstrument
    totalRequestTime { hours }
  }
  flamingos2Imaging { filters { filter } }
  flamingos2LongSlit { disperser filter fpu }
  ghostIfu { resolutionMode }
  gmosNorthImaging { filters { filter } }
  gmosNorthLongSlit { grating filter fpu centralWavelength { nanometers } }
  gmosNorthMos { grating filter customMask { slitWidth } centralWavelength { nanometers } }
  gmosSouthImaging { filters { filter } }
  gmosSouthLongSlit { grating filter fpu  centralWavelength { nanometers } }
  gnirsImaging { filters { filter } camera }
  gnirsSpectroscopy {
    grating prism camera slit { fpu } ifu { fpu } filter
    centralWavelengths { centralWavelength { nanometers } }
  }
  igrins2LongSlit { telluricType { tag } }
  visitor { mode centralWavelength { nanometers } name }
}

fragment Execution on Execution {
  digest {
    calculationState
    value {
      estimate { total { total { hours } } }
      science { observeClass }
    }
  }
}

fragment Target on Target {
  id
  name
  disposition
  calibrationRole
  sourceProfile {
    point {
      bandNormalized {
        brightnesses { band value units }
        sed { ...UnnormalizedSed }
      }
      emissionLines { ...EmissionLinesIntegrated }
    }
    uniform {
      bandNormalized {
        brightnesses { band value units }
        sed { ...UnnormalizedSed }
      }
      emissionLines {
        lines {
          wavelength { nanometers }
          lineWidth
          lineFlux { value units }
        }
        fluxDensityContinuum { value units }
      }
    }
    gaussian {
      fwhm { arcseconds }
      bandNormalized {
        brightnesses { band value units }
        sed { ...UnnormalizedSed }
      }
      emissionLines { ...EmissionLinesIntegrated }
    }
  }
  sidereal {
    ra { degrees }
    dec { degrees }
    epoch
  }
  nonsidereal {
    des
    keyType
    key
  }
  opportunity {
    region {
      rightAscensionArc {
        type
        start { degrees }
        end { degrees }
      }
      declinationArc {
        type
        start { degrees }
        end { degrees }
      }
    }
  }
}

fragment EmissionLinesIntegrated on EmissionLinesIntegrated {
  lines {
    wavelength { nanometers }
    lineWidth
    lineFlux { value units }
  }
  fluxDensityContinuum { value units }
}

fragment UnnormalizedSed on UnnormalizedSed {
  stellarLibrary
  coolStar
  galaxy
  planet
  quasar
  hiiRegion
  planetaryNebula
  powerLaw
  blackBodyTempK
  fluxDensities { wavelength { nanometers } density }
}

fragment ItcObservation on Observation {
  id
  reference { label }
  instrument
  calibrationRole
  workflow { value { state } }
  targetEnvironment {
    firstScienceTarget {
      name
      sourceProfile { ...sourceProfileFields }
      sidereal {
        ra { hms }
        dec { dms }
        radialVelocity { kilometersPerSecond }
      }
    }
  }
  constraintSet { ...constraintSetFields }
  observingMode {
    instrument
    mode
    flamingos2LongSlit { disperser filter fpu exposureTimeMode { ...exposureTimeModeFields } }
    flamingos2Mos { disperser filter customMask { attachmentId slitWidth } exposureTimeMode { ...exposureTimeModeFields } }
    flamingos2Imaging { filters { filter exposureTimeMode { ...exposureTimeModeFields } } }
    ghostIfu {
      stepCount resolutionMode
      red { readMode binning exposureTimeMode { ...exposureTimeModeFields } }
      blue { readMode binning exposureTimeMode { ...exposureTimeModeFields } }
    }
    gmosNorthImaging { bin ampGain ampReadMode filters { filter exposureTimeMode { ...exposureTimeModeFields } } }
    gmosNorthLongSlit {
      grating filter fpu roi xBin yBin ampGain ampReadMode
      centralWavelength { nanometers } exposureTimeMode { ...exposureTimeModeFields }
    }
    gmosNorthMos {
      grating filter customMask { attachmentId slitWidth } roi xBin yBin ampGain ampReadMode
      centralWavelength { nanometers } exposureTimeMode { ...exposureTimeModeFields }
    }
    gmosSouthImaging { bin ampGain ampReadMode filters { filter exposureTimeMode { ...exposureTimeModeFields } } }
    gmosSouthLongSlit {
      grating filter fpu roi xBin yBin ampGain ampReadMode
      centralWavelength { nanometers } exposureTimeMode { ...exposureTimeModeFields }
    }
    gmosSouthMos {
      grating filter customMask { attachmentId slitWidth } roi xBin yBin ampGain ampReadMode
      centralWavelength { nanometers } exposureTimeMode { ...exposureTimeModeFields }
    }
    gnirsSpectroscopy {
      grating prism camera filter wellDepth slit { fpu } ifu { fpu }
      centralWavelengths {
        coadds
        centralWavelength { nanometers micrometers }
        exposureTimeMode { ...exposureTimeModeFields }
      }
    }
    gnirsImaging { camera wellDepth filters { filter coadds exposureTimeMode { ...exposureTimeModeFields } } }
    igrins2LongSlit { exposureTimeMode { ...exposureTimeModeFields } }
  }
  execution {
    digest { value { estimate { total { total { minutes } } } } }
  }
}

fragment sourceProfileFields on SourceProfile {
  point {
    bandNormalized { ...bandNormalizedFields }
    emissionLines { ...emissionLinesIntegratedFields }
  }
  uniform {
    bandNormalized { ...bandNormalizedSurfaceFields }
    emissionLines { ...emissionLinesSurfaceFields }
  }
  gaussian {
    fwhm { microarcseconds }
    bandNormalized { ...bandNormalizedFields }
    emissionLines { ...emissionLinesIntegratedFields }
  }
}

fragment bandNormalizedFields on BandNormalizedIntegrated {
  sed { ...sedFields }
  brightnesses { band value units error }
}

fragment bandNormalizedSurfaceFields on BandNormalizedSurface {
  sed { ...sedFields }
  brightnesses { band value units error }
}

fragment sedFields on UnnormalizedSed {
  stellarLibrary coolStar galaxy planet quasar hiiRegion planetaryNebula
  powerLaw blackBodyTempK fluxDensitiesAttachment
  fluxDensities { wavelength { nanometers } density }
}

fragment emissionLinesIntegratedFields on EmissionLinesIntegrated {
  lines {
    wavelength { nanometers }
    lineWidth
    lineFlux { value units }
  }
  fluxDensityContinuum { value units error }
}

fragment emissionLinesSurfaceFields on EmissionLinesSurface {
  lines {
    wavelength { nanometers }
    lineWidth
    lineFlux { value units }
  }
  fluxDensityContinuum { value units error }
}

fragment exposureTimeModeFields on ExposureTimeMode {
  signalToNoise { value at { nanometers } }
  timeAndCount  { time { seconds } count at { nanometers } }
}

fragment constraintSetFields on ConstraintSet {
  imageQuality
  cloudExtinction
  skyBackground
  waterVapor
  elevationRange {
    airMass   { min max }
    hourAngle { minHours maxHours }
  }
}
"""
