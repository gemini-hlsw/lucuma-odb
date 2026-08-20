// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import lucuma.core.model.Observation
import lucuma.core.model.User

// sc-9893: Reproduces the Postgres 54011 "Target lists can have at most 1664
// entries" error
// A selection covering the full observing mode of every
// instrument makes grackle join v_offset_generator (and its nested
// v_enumerated_offset) once per imaging mode/variant/role path
// so the full ObservingMode selection exceeds the Postgres target-list limit in the
// single generated statement.
//
// The test reproduces a query sent by explore
class ShortCut_9893 extends OdbSuite {

  val pi: User = TestUsers.Standard.pi(1, 30)
  val validUsers: List[User] = List(pi)

  val generatorFields: String = """
    generatorType
    enumerated {
      values {
        offset {
          p { microarcseconds }
          q { microarcseconds }
        }
        guiding
      }
    }
    random {
      size { microarcseconds }
      center {
        p { microarcseconds }
        q { microarcseconds }
      }
      seed
    }
    spiral {
      size { microarcseconds }
      center {
        p { microarcseconds }
        q { microarcseconds }
      }
      seed
    }
    uniform {
      cornerA {
        p { microarcseconds }
        q { microarcseconds }
      }
      cornerB {
        p { microarcseconds }
        q { microarcseconds }
      }
    }
  """

  val variantFields: String = s"""
    variantType
    grouped {
      order
      skyCount
      offsets { $generatorFields }
      skyOffsets { $generatorFields }
    }
    interleaved {
      skyCount
      offsets { $generatorFields }
      skyOffsets { $generatorFields }
    }
    preImaging {
      offset1 { p { microarcseconds } q { microarcseconds } }
      offset2 { p { microarcseconds } q { microarcseconds } }
      offset3 { p { microarcseconds } q { microarcseconds } }
      offset4 { p { microarcseconds } q { microarcseconds } }
    }
  """

  val exposureTimeModeFields: String = """
    exposureTimeMode {
      signalToNoise {
        value
        at { nanometers }
      }
      timeAndCount {
        time { seconds }
        count
        at { nanometers }
      }
    }
  """

  val offsetFields: String = """
    p { microarcseconds }
    q { microarcseconds }
  """

  val slitTelescopeConfigsFields: String = s"""
    offsetMode
    alongSlit {
      q { microarcseconds }
      guiding
    }
    toSky {
      offset { $offsetFields }
      guiding
    }
  """

  val gmosLongSlitFields: String = s"""
    grating
    filter
    fpu
    centralWavelength { nanometers }
    $exposureTimeModeFields
    xBin
    defaultXBin
    explicitXBin
    yBin
    defaultYBin
    explicitYBin
    ampReadMode
    defaultAmpReadMode
    explicitAmpReadMode
    ampGain
    defaultAmpGain
    explicitAmpGain
    roi
    defaultRoi
    explicitRoi
    wavelengthDithers { picometers }
    defaultWavelengthDithers { picometers }
    explicitWavelengthDithers { picometers }
    offsets { microarcseconds }
    defaultOffsets { microarcseconds }
    explicitOffsets { microarcseconds }
    acquisition {
      filter
      defaultFilter
      explicitFilter
      roi
      defaultRoi
      explicitRoi
      $exposureTimeModeFields
    }
    initialGrating
    initialFilter
    initialFpu
    initialCentralWavelength { nanometers }
  """

  val gmosMosFields: String = s"""
    grating
    filter
    customMask {
      attachmentId
      slitWidth
    }
    centralWavelength { nanometers }
    acquisitionType
    $exposureTimeModeFields
    xBin
    defaultXBin
    explicitXBin
    yBin
    defaultYBin
    explicitYBin
    ampReadMode
    defaultAmpReadMode
    explicitAmpReadMode
    ampGain
    defaultAmpGain
    explicitAmpGain
    roi
    defaultRoi
    explicitRoi
    wavelengthDithers { picometers }
    defaultWavelengthDithers { picometers }
    explicitWavelengthDithers { picometers }
    offsets { microarcseconds }
    defaultOffsets { microarcseconds }
    explicitOffsets { microarcseconds }
    initialGrating
    initialFilter
    initialSlitWidth
    initialCentralWavelength { nanometers }
    acquisition {
      filter
      defaultFilter
      explicitFilter
      $exposureTimeModeFields
    }
  """

  def wideQuery(oid: Observation.Id): String = s"""
    query {
      observation(observationId: "$oid") {
        observingMode {
          gmosNorthImaging {
            variant { $variantFields }
            filters { filter $exposureTimeModeFields }
            initialFilters { filter $exposureTimeModeFields }
            bin
            explicitBin
            defaultBin
            ampReadMode
            explicitAmpReadMode
            defaultAmpReadMode
            ampGain
            explicitAmpGain
            defaultAmpGain
            roi
            explicitRoi
            defaultRoi
          }
          gmosSouthImaging {
            variant { $variantFields }
            filters { filter $exposureTimeModeFields }
            initialFilters { filter $exposureTimeModeFields }
            bin
            explicitBin
            defaultBin
            ampReadMode
            explicitAmpReadMode
            defaultAmpReadMode
            ampGain
            explicitAmpGain
            defaultAmpGain
            roi
            explicitRoi
            defaultRoi
          }
          flamingos2Imaging {
            variant { $variantFields }
            filters { filter $exposureTimeModeFields }
            initialFilters { filter $exposureTimeModeFields }
            explicitReadMode
            defaultReadMode
            explicitReads
            defaultReads
            decker
            explicitDecker
            defaultDecker
            readoutMode
            explicitReadoutMode
            defaultReadoutMode
          }
          gnirsImaging {
            variant { $variantFields }
            filters { filter $exposureTimeModeFields coadds }
            initialFilters { filter $exposureTimeModeFields coadds }
            camera
            explicitReadMode
            wellDepth
            explicitWellDepth
            defaultWellDepth
          }
          gmosNorthLongSlit { $gmosLongSlitFields }
          gmosSouthLongSlit { $gmosLongSlitFields }
          gmosNorthMos { $gmosMosFields }
          gmosSouthMos { $gmosMosFields }
          flamingos2LongSlit {
            disperser
            filter
            fpu
            $exposureTimeModeFields
            explicitReadMode
            explicitReads
            decker
            defaultDecker
            explicitDecker
            readoutMode
            defaultReadoutMode
            explicitReadoutMode
            telescopeConfigs { $slitTelescopeConfigsFields }
            defaultTelescopeConfigs { $slitTelescopeConfigsFields }
            explicitTelescopeConfigs { $slitTelescopeConfigsFields }
            telluricType { tag starTypes }
            acquisition {
              filter
              defaultFilter
              explicitFilter
              $exposureTimeModeFields
            }
            initialDisperser
            initialFilter
            initialFpu
          }
          gnirsSpectroscopy {
            grating
            explicitGrating
            initialGrating
            prism
            explicitPrism
            initialPrism
            centralWavelengths {
              centralWavelength { nanometers }
              $exposureTimeModeFields
              coadds
            }
            initialCentralWavelengths {
              centralWavelength { nanometers }
              $exposureTimeModeFields
              coadds
            }
            camera
            initialCamera
            slit {
              fpu
              initialFpu
              telescopeConfigs { $slitTelescopeConfigsFields }
              defaultTelescopeConfigs { $slitTelescopeConfigsFields }
              explicitTelescopeConfigs { $slitTelescopeConfigsFields }
            }
            ifu {
              fpu
              initialFpu
              telescopeConfigs {
                offset { $offsetFields }
                guiding
              }
            }
            filter
            initialFilter
            decker
            explicitDecker
            defaultDecker
            explicitReadMode
            wellDepth
            explicitWellDepth
            defaultWellDepth
            explicitFocusMotorSteps
            acquisition {
              $exposureTimeModeFields
              coadds
              explicitAcquisitionType
              explicitFilter
              skyOffset { $offsetFields }
            }
            telluricType { tag starTypes }
          }
          igrins2LongSlit {
            $exposureTimeModeFields
            svc {
              exposure { seconds }
              defaultExposure { seconds }
              explicitExposure { seconds }
              telescopeConfigs {
                offset { $offsetFields }
                guiding
              }
              defaultTelescopeConfigs {
                offset { $offsetFields }
                guiding
              }
              explicitTelescopeConfigs {
                offset { $offsetFields }
                guiding
              }
            }
            telescopeConfigs { $slitTelescopeConfigsFields }
            defaultTelescopeConfigs { $slitTelescopeConfigsFields }
            explicitTelescopeConfigs { $slitTelescopeConfigsFields }
            telluricType { tag starTypes }
          }
          ghostIfu {
            stepCount
            resolutionMode
            red {
              $exposureTimeModeFields
              binning
              defaultBinning
              explicitBinning
              readMode
              defaultReadMode
              explicitReadMode
            }
            blue {
              $exposureTimeModeFields
              binning
              defaultBinning
              explicitBinning
              readMode
              defaultReadMode
              explicitReadMode
            }
            skyPosition {
              ra { degrees }
              dec { degrees }
            }
            slitViewingCameraExposureTime { seconds }
            ifu1Agitator
            defaultIfu1Agitator
            explicitIfu1Agitator
            ifu2Agitator
            defaultIfu2Agitator
            explicitIfu2Agitator
          }
        }
      }
    }
  """

  test("wide imaging observing mode selection does not exceed the Postgres column limit") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        tid <- createTargetAs(pi, pid)
        oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
      } yield oid

    setup.flatMap { oid =>
      query(pi, wideQuery(oid)).map { js =>
        val variantType =
          js.hcursor
            .downFields("observation", "observingMode", "gmosNorthImaging", "variant", "variantType")
            .as[String]
        assert(variantType.isRight, s"unexpected response: ${js.spaces2}")
      }
    }
  }
}
