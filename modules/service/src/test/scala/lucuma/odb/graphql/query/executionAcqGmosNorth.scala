// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.GmosLongSlitAcquisitionRoi
import lucuma.core.enums.GmosLongSlitAcquisitionRoi.*
import lucuma.core.enums.Instrument
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ImagingInput

class executionAcqGmosNorth extends ExecutionTestSupportForGmos:

  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.exposureTimeMode match
      case ExposureTimeMode.TimeAndCountMode(t, c, _) => IntegrationTime(t, c).some
      case _ => none

  def initialAcquisition(
    roi: GmosLongSlitAcquisitionRoi = Ccd2
  ): Json =
    json"""
      {
        "executionConfig": {
          "gmosNorth": {
            "acquisition": {
              "nextAtom": {
                "description": "Initial Acquisition",
                "observeClass": "ACQUISITION",
                "steps": [
                  ${gmosNorthExpectedAcq(0,  0, roi = roi)},
                  ${gmosNorthExpectedAcq(1, 10, roi = roi)},
                  ${gmosNorthExpectedAcq(2,  0, roi = roi, Breakpoint.Enabled)}
                ]
              },
              "possibleFuture": [
                {
                  "description": "Fine Adjustments",
                  "observeClass": "ACQUISITION",
                  "steps": [
                    ${gmosNorthExpectedAcq(2, 0, roi = roi)}
                  ]
                }
              ],
              "hasMore": false
            }
          }
        }
      }
    """

  def fineAdjustments(
    roi: GmosLongSlitAcquisitionRoi = Ccd2
  ): Json =
    json"""
      {
        "executionConfig": {
          "gmosNorth": {
            "acquisition": {
              "nextAtom": {
                "description": "Fine Adjustments",
                "observeClass": "ACQUISITION",
                "steps": [
                  ${gmosNorthExpectedAcq(2, 0, roi = roi, Breakpoint.Disabled)}
                ]
              },
              "possibleFuture": [
                {
                  "description": "Fine Adjustments",
                  "observeClass": "ACQUISITION",
                  "steps": [
                    ${gmosNorthExpectedAcq(2, 0, roi = roi, Breakpoint.Disabled)}
                  ]
                }
              ],
              "hasMore": false
            }
          }
        }
      }
    """

  test("initial generation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected = initialAcquisition(Ccd2).asRight
      )

  test("execute first step only, reset"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Execute the first step.
        s  <- firstAcquisitionStepId(serviceUser, o)
        _  <- addEndStepEvent(s, v)
        _  <- resetAcquisitionAs(serviceUser, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected = initialAcquisition(Ccd2).asRight
      )

  test("execute first step only"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Execute the first step.
        s  <- firstAcquisitionStepId(serviceUser, o)
        _  <- addEndStepEvent(s, v)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected =
          json"""
            {
              "executionConfig": {
                "gmosNorth": {
                  "acquisition": {
                    "nextAtom": {
                      "description": "Initial Acquisition",
                      "observeClass": "ACQUISITION",
                      "steps": [
                        ${gmosNorthExpectedAcq(1, 10, roi = Ccd2)},
                        ${gmosNorthExpectedAcq(2,  0, roi = Ccd2, breakpoint = Breakpoint.Enabled)}
                      ]
                    },
                    "possibleFuture": [
                      {
                        "description": "Fine Adjustments",
                        "observeClass": "ACQUISITION",
                        "steps": [
                          ${gmosNorthExpectedAcq(2, 0, roi = Ccd2, breakpoint = Breakpoint.Disabled)}
                        ]
                      }
                    ],
                    "hasMore": false
                  }
                }
              }
            }
          """.asRight
      )

  test("execute acquisition, switch to science, back to acquisition"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Acquisition Sequence
        ac <- acquisitionStepIds(serviceUser, o)
        _  <- ac.traverse(sid => addEndStepEvent(sid, v))

        // Do a science step
        s  <- firstScienceStepId(serviceUser, o)
        _  <- addEndStepEvent(s, v)

        // Reset acquisition to take it from the top.
        _  <- resetAcquisitionAs(serviceUser, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected = initialAcquisition(Ccd2).asRight
      )

  test("science step ids do not change while executing acquisition"):
    val execAcq: IO[Set[Step.Id]] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        x0 <- firstScienceStepId(serviceUser, o)

        a  <- acquisitionStepIds(serviceUser, o)
        _  <- addEndStepEvent(a(0), v)

        x1 <- firstScienceStepId(serviceUser, o)

        _  <- addEndStepEvent(a(1), v)

        x1 <- firstScienceStepId(serviceUser, o)

        _  <- addEndStepEvent(a(2), v)

        x2 <- firstScienceStepId(serviceUser, o)

        _  <- addEndStepEvent(a(3), v)

        x3 <- firstScienceStepId(serviceUser, o)
      yield Set(x0, x1, x2, x3)

    assertIO(execAcq.map(_.size), 1)

  test("reset can only be done by staff or better"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _ <- expect(
        user  = pi,
        query = s"""
          mutation {
            resetAcquisition(input: {
              observationId: "$o"
            }) {
              observation { id }
            }
          }
        """,
        expected = List(
          s"User ${pi.id} is not authorized to perform this operation."
        ).asLeft
      )
    yield ()

  test("override exposure time"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), s"""
               gmosNorthLongSlit: {
                 grating: R831_G5302
                 filter: R_PRIME
                 fpu: LONG_SLIT_0_50
                 centralWavelength: {
                   nanometers: 500
                 }
                 explicitYBin: TWO
                 acquisition: {
                   exposureTimeMode: {
                     timeAndCount: {
                       time: { seconds: 16 }
                       count: 1
                       at: { nanometers: 500 }
                     }
                   }
                 }
              }
             """)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = s"""
          query {
            executionConfig(observationId: "$oid") {
              gmosNorth {
                acquisition {
                  nextAtom {
                    steps {
                      instrumentConfig {
                        exposure { seconds }
                      }
                    }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "executionConfig": {
              "gmosNorth": {
                "acquisition": {
                  "nextAtom": {
                    "steps": [
                      {
                        "instrumentConfig": {
                          "exposure": { "seconds": 16.000000 }
                        }
                      },
                      {
                        "instrumentConfig": {
                          "exposure": { "seconds": 20.000000 }
                        }
                      },
                      {
                        "instrumentConfig": {
                          "exposure": { "seconds": 48.000000 }
                        }
                      }
                    ]
                  }
                }
              }
            }
          }
        """.asRight
      )

  test("invalid GMOS North acquisition filter"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      _ <- expect(
             user  = pi,
             query = createObservationWithModeQuery(p, List(t), s"""
               gmosNorthLongSlit: {
                 grating: R831_G5302
                 filter: R_PRIME
                 fpu: LONG_SLIT_0_50
                 centralWavelength: {
                   nanometers: 500
                 }
                 explicitYBin: TWO
                 acquisition: {
                   explicitFilter: GG455
                 }
               }
             """),
             expected = List(
               "Argument 'input.SET.observingMode.gmosNorthLongSlit.acquisition' is invalid: 'explicitFilter' must contain one of: G_PRIME, R_PRIME, I_PRIME, Z_PRIME"
             ).asLeft
           )
    yield ()

  test("override acquisition filter"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), s"""
               gmosNorthLongSlit: {
                 grating: R831_G5302
                 filter: R_PRIME
                 fpu: LONG_SLIT_0_50
                 centralWavelength: {
                   nanometers: 500
                 }
                 explicitYBin: TWO
                 acquisition: {
                   explicitFilter: Z_PRIME
                 }
              }
             """)
        _ <- expect(
          user  = pi,
          query = s"""
            query {
              observation(observationId: "$o") {
                observingMode {
                  gmosNorthLongSlit {
                    acquisition {
                      filter
                      defaultFilter
                      explicitFilter
                    }
                  }
                }
              }
            }
          """,
          expected = json"""
            {
              "observation": {
                "observingMode": {
                  "gmosNorthLongSlit": {
                    "acquisition": {
                      "filter": "Z_PRIME",
                      "defaultFilter": "G_PRIME",
                      "explicitFilter": "Z_PRIME"
                    }
                  }
                }
              }
            }
          """.asRight
        )
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = s"""
          query {
            executionConfig(observationId: "$oid") {
              gmosNorth {
                acquisition {
                  nextAtom {
                    steps {
                      instrumentConfig {
                        filter
                      }
                    }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "executionConfig": {
              "gmosNorth": {
                "acquisition": {
                  "nextAtom": {
                    "steps": [
                      {
                        "instrumentConfig": {
                          "filter": "Z_PRIME"
                        }
                      },
                      {
                        "instrumentConfig": {
                          "filter": "Z_PRIME"
                        }
                      },
                      {
                        "instrumentConfig": {
                          "filter": "Z_PRIME"
                        }
                      }
                    ]
                  }
                }
              }
            }
          }
        """.asRight
      )

  test("override roi"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), s"""
               gmosNorthLongSlit: {
                 grating: R831_G5302
                 filter: R_PRIME
                 fpu: LONG_SLIT_0_50
                 centralWavelength: {
                   nanometers: 500
                 }
                 explicitYBin: TWO
                 acquisition: {
                   explicitRoi: FULL_CCD2
                 }
              }
             """)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected = initialAcquisition(FullCcd2).asRight
      )