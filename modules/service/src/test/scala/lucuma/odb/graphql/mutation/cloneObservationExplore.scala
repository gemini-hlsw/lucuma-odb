// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.Clock
import cats.effect.IO
import lucuma.core.model.Observation
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

import scala.concurrent.duration.*

class cloneObservationExplore extends ExecutionTestSupportForGmos {

  override def munitIOTimeout = 2.minutes
  override protected def serverResponseTimeout = 2.minutes

  // The full Explore cloneObservation mutation with all fields inlined.
  def exploreCloneMutation(oid: Observation.Id): String = s"""
    mutation {
      cloneObservation(input: { observationId: "$oid" }) {
        newObservation {
          id
          title
          subtitle
          observationTime
          observationDuration {
            microseconds
          }

          posAngleConstraint {
            mode
            angle {
              microarcseconds
            }
          }

          targetEnvironment {
            asterism {
              id
            }
            guideTargetName
            useBlindOffset
            blindOffsetTarget {
              id
            }
            blindOffsetType
          }
          constraintSet {
            cloudExtinction
            imageQuality
            skyBackground
            waterVapor
            elevationRange {
              airMass {
                min
                max
              }
              hourAngle {
                minHours
                maxHours
              }
            }
          }

          timingWindows {
            inclusion
            startUtc
            end {
              ... on TimingWindowEndAt {
                atUtc
              }
              ... on TimingWindowEndAfter {
                after {
                  milliseconds
                }
                repeat {
                  period {
                    milliseconds
                  }
                  times
                }
              }
            }
          }

          attachments {
            id
          }
          scienceRequirements {
            exposureTimeMode {
              signalToNoise {
                value
                at {
                  picometers
                }
              }
              timeAndCount {
                time {
                  microseconds
                }

                count
                at {
                  picometers
                }
              }
            }

            spectroscopy {
              wavelength {
                picometers
              }

              resolution
              wavelengthCoverage {
                picometers
              }

              focalPlane
              focalPlaneAngle {
                microarcseconds
              }

              capability
            }
            imaging {
              minimumFov {
                microarcseconds
              }

              narrowFilters
              broadFilters
              combinedFilters
            }
          }
          observingMode {
            gmosNorthLongSlit {
              initialGrating
              initialFilter
              initialFpu
              initialCentralWavelength {
                picometers
              }

              grating
              filter
              fpu
              centralWavelength {
                picometers
              }

              defaultXBin
              explicitXBin
              defaultYBin
              explicitYBin
              defaultAmpReadMode
              explicitAmpReadMode
              defaultAmpGain
              explicitAmpGain
              defaultRoi
              explicitRoi
              defaultWavelengthDithers {
                picometers
              }

              explicitWavelengthDithers {
                picometers
              }

              defaultOffsets {
                microarcseconds
              }

              explicitOffsets {
                microarcseconds
              }

              exposureTimeMode {
                signalToNoise {
                  value
                  at {
                    picometers
                  }
                }
                timeAndCount {
                  time {
                    microseconds
                  }

                  count
                  at {
                    picometers
                  }
                }
              }

              acquisition {
                defaultFilter
                explicitFilter
                defaultRoi
                explicitRoi
                exposureTimeMode {
                  signalToNoise {
                    value
                    at {
                      picometers
                    }
                  }
                  timeAndCount {
                    time {
                      microseconds
                    }

                    count
                    at {
                      picometers
                    }
                  }
                }
              }
            }
            gmosSouthLongSlit {
              initialGrating
              initialFilter
              initialFpu
              initialCentralWavelength {
                picometers
              }

              grating
              filter
              fpu
              centralWavelength {
                picometers
              }

              defaultXBin
              explicitXBin
              defaultYBin
              explicitYBin
              defaultAmpReadMode
              explicitAmpReadMode
              defaultAmpGain
              explicitAmpGain
              defaultRoi
              explicitRoi
              defaultWavelengthDithers {
                picometers
              }

              explicitWavelengthDithers {
                picometers
              }

              defaultOffsets {
                microarcseconds
              }

              explicitOffsets {
                microarcseconds
              }

              exposureTimeMode {
                signalToNoise {
                  value
                  at {
                    picometers
                  }
                }
                timeAndCount {
                  time {
                    microseconds
                  }

                  count
                  at {
                    picometers
                  }
                }
              }

              acquisition {
                defaultFilter
                explicitFilter
                defaultRoi
                explicitRoi
                exposureTimeMode {
                  signalToNoise {
                    value
                    at {
                      picometers
                    }
                  }
                  timeAndCount {
                    time {
                      microseconds
                    }

                    count
                    at {
                      picometers
                    }
                  }
                }
              }
            }
            gmosNorthImaging {
              variant {
                variantType
                grouped {
                  order
                  offsets {
                    generatorType
                    enumerated {
                      values {
                        offset {
                          p {
                            microarcseconds
                          }
                          q {
                            microarcseconds
                          }
                        }

                        guiding
                      }
                    }
                    random {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    spiral {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    uniform {
                      cornerA {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }

                      cornerB {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                  }

                  skyCount
                  skyOffsets {
                    generatorType
                    enumerated {
                      values {
                        offset {
                          p {
                            microarcseconds
                          }
                          q {
                            microarcseconds
                          }
                        }

                        guiding
                      }
                    }
                    random {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    spiral {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    uniform {
                      cornerA {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }

                      cornerB {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                  }
                }
                interleaved {
                  offsets {
                    generatorType
                    enumerated {
                      values {
                        offset {
                          p {
                            microarcseconds
                          }
                          q {
                            microarcseconds
                          }
                        }

                        guiding
                      }
                    }
                    random {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    spiral {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    uniform {
                      cornerA {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }

                      cornerB {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                  }

                  skyCount
                  skyOffsets {
                    generatorType
                    enumerated {
                      values {
                        offset {
                          p {
                            microarcseconds
                          }
                          q {
                            microarcseconds
                          }
                        }

                        guiding
                      }
                    }
                    random {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    spiral {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    uniform {
                      cornerA {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }

                      cornerB {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                  }
                }
                preImaging {
                  offset1 {
                    p {
                      microarcseconds
                    }
                    q {
                      microarcseconds
                    }
                  }

                  offset2 {
                    p {
                      microarcseconds
                    }
                    q {
                      microarcseconds
                    }
                  }

                  offset3 {
                    p {
                      microarcseconds
                    }
                    q {
                      microarcseconds
                    }
                  }

                  offset4 {
                    p {
                      microarcseconds
                    }
                    q {
                      microarcseconds
                    }
                  }
                }
              }

              initialFilters {
                filter
                exposureTimeMode {
                  signalToNoise {
                    value
                    at {
                      picometers
                    }
                  }
                  timeAndCount {
                    time {
                      microseconds
                    }

                    count
                    at {
                      picometers
                    }
                  }
                }
              }
              filters {
                filter
                exposureTimeMode {
                  signalToNoise {
                    value
                    at {
                      picometers
                    }
                  }
                  timeAndCount {
                    time {
                      microseconds
                    }

                    count
                    at {
                      picometers
                    }
                  }
                }
              }
              defaultBin
              explicitBin
              defaultAmpReadMode
              explicitAmpReadMode
              defaultAmpGain
              explicitAmpGain
              defaultRoi
              explicitRoi
            }
            gmosSouthImaging {
              variant {
                variantType
                grouped {
                  order
                  offsets {
                    generatorType
                    enumerated {
                      values {
                        offset {
                          p {
                            microarcseconds
                          }
                          q {
                            microarcseconds
                          }
                        }

                        guiding
                      }
                    }
                    random {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    spiral {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    uniform {
                      cornerA {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }

                      cornerB {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                  }

                  skyCount
                  skyOffsets {
                    generatorType
                    enumerated {
                      values {
                        offset {
                          p {
                            microarcseconds
                          }
                          q {
                            microarcseconds
                          }
                        }

                        guiding
                      }
                    }
                    random {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    spiral {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    uniform {
                      cornerA {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }

                      cornerB {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                  }
                }
                interleaved {
                  offsets {
                    generatorType
                    enumerated {
                      values {
                        offset {
                          p {
                            microarcseconds
                          }
                          q {
                            microarcseconds
                          }
                        }

                        guiding
                      }
                    }
                    random {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    spiral {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    uniform {
                      cornerA {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }

                      cornerB {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                  }

                  skyCount
                  skyOffsets {
                    generatorType
                    enumerated {
                      values {
                        offset {
                          p {
                            microarcseconds
                          }
                          q {
                            microarcseconds
                          }
                        }

                        guiding
                      }
                    }
                    random {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    spiral {
                      size {
                        microarcseconds
                      }

                      center {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                    uniform {
                      cornerA {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }

                      cornerB {
                        p {
                          microarcseconds
                        }
                        q {
                          microarcseconds
                        }
                      }
                    }
                  }
                }
                preImaging {
                  offset1 {
                    p {
                      microarcseconds
                    }
                    q {
                      microarcseconds
                    }
                  }

                  offset2 {
                    p {
                      microarcseconds
                    }
                    q {
                      microarcseconds
                    }
                  }

                  offset3 {
                    p {
                      microarcseconds
                    }
                    q {
                      microarcseconds
                    }
                  }

                  offset4 {
                    p {
                      microarcseconds
                    }
                    q {
                      microarcseconds
                    }
                  }
                }
              }

              initialFilters {
                filter
                exposureTimeMode {
                  signalToNoise {
                    value
                    at {
                      picometers
                    }
                  }
                  timeAndCount {
                    time {
                      microseconds
                    }

                    count
                    at {
                      picometers
                    }
                  }
                }
              }
              filters {
                filter
                exposureTimeMode {
                  signalToNoise {
                    value
                    at {
                      picometers
                    }
                  }
                  timeAndCount {
                    time {
                      microseconds
                    }

                    count
                    at {
                      picometers
                    }
                  }
                }
              }
              defaultBin
              explicitBin
              defaultAmpReadMode
              explicitAmpReadMode
              defaultAmpGain
              explicitAmpGain
              defaultRoi
              explicitRoi
            }
            flamingos2LongSlit {
              initialDisperser
              initialFilter
              initialFpu
              disperser
              filter
              fpu
              explicitReadMode
              explicitReads
              defaultDecker
              explicitDecker
              defaultReadoutMode
              explicitReadoutMode
              defaultOffsets {
                p {
                  microarcseconds
                }
                q {
                  microarcseconds
                }
              }

              explicitOffsets {
                p {
                  microarcseconds
                }
                q {
                  microarcseconds
                }
              }

              exposureTimeMode {
                signalToNoise {
                  value
                  at {
                    picometers
                  }
                }
                timeAndCount {
                  time {
                    microseconds
                  }

                  count
                  at {
                    picometers
                  }
                }
              }

              acquisition {
                exposureTimeMode {
                  signalToNoise {
                    value
                    at {
                      picometers
                    }
                  }
                  timeAndCount {
                    time {
                      microseconds
                    }

                    count
                    at {
                      picometers
                    }
                  }
                }
              }
            }
            igrins2LongSlit {
              exposureTimeMode {
                signalToNoise {
                  value
                  at {
                    picometers
                  }
                }
                timeAndCount {
                  time {
                    microseconds
                  }

                  count
                  at {
                    picometers
                  }
                }
              }

              defaultOffsetMode
              explicitOffsetMode
              defaultSaveSVCImages
              explicitSaveSVCImages
            }
          }

          observerNotes
          calibrationRole
          scienceBand
          configuration {
            conditions {
              imageQuality
              cloudExtinction
              skyBackground
              waterVapor
            }
            target {
              coordinates {
                ra {
                  microseconds
                }

                dec {
                  microarcseconds
                }
              }
              region {
                rightAscensionArc {
                  type
                  start {
                    microseconds
                  }

                  end {
                    microseconds
                  }
                }
                declinationArc {
                  type
                  start {
                    microarcseconds
                  }

                  end {
                    microarcseconds
                  }
                }
              }
            }
            observingMode {
              instrument
              mode
              gmosNorthLongSlit {
                grating
              }
              gmosSouthLongSlit {
                grating
              }
              flamingos2LongSlit {
                disperser
              }
              gmosNorthImaging {
                filters
              }
              gmosSouthImaging {
                filters
              }
            }
          }

          configurationRequests {
            id
          }
          workflow {
            calculationState
            value {
              state
              validTransitions
              validationErrors {
                code
                messages
              }
            }
          }

          groupId
          groupIndex
          reference {
            label
          }
          execution {
            digest {
              calculationState
              value {
                setup {
                  full {
                    microseconds
                  }

                  reacquisition {
                    microseconds
                  }
                }
                acquisition {
                  observeClass
                  atomCount
                  timeEstimate {
                    program {
                      microseconds
                    }

                    nonCharged {
                      microseconds
                    }

                    total {
                      microseconds
                    }
                  }
                  telescopeConfigs {
                    offset {
                      p {
                        microarcseconds
                      }
                      q {
                        microarcseconds
                      }
                    }

                    guiding
                  }
                  executionState
                }

                science {
                  observeClass
                  atomCount
                  timeEstimate {
                    program {
                      microseconds
                    }

                    nonCharged {
                      microseconds
                    }

                    total {
                      microseconds
                    }
                  }
                  telescopeConfigs {
                    offset {
                      p {
                        microarcseconds
                      }
                      q {
                        microarcseconds
                      }
                    }

                    guiding
                  }
                  executionState
                }
              }
            }

            timeCharge {
              program {
                microseconds
              }
            }
          }
        }
      }
    }
  """

  test("clone with query used by explore") {
    // to see the query on stdout, update
    //   <logger name="lucuma-odb-test" level="DEBUG" />
    // enabling the logger impacts how long it takes to run the mutation
    // on my local tests with logger it takes around 30s while without it takes around 2s
    for {
      pid   <- createProgramAs(pi)
      tid   <- createTargetWithProfileAs(pi, pid)
      oid   <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _     <- runObscalcUpdate(pid, oid)
      t0    <- Clock[IO].monotonic
      ior   <- queryIor(user = pi, query = exploreCloneMutation(oid))
      t1    <- Clock[IO].monotonic
      _     <- IO.println(s"clone mutation took ${(t1 - t0).toMillis} ms")
      json  <- IO.println(ior.toOption.flatMap(_.asObject).map(_.toJson.spaces2))
    } yield ()
  }

}
