// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObserveClass
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.json.time.decoder.given
import lucuma.odb.json.timeaccounting.given

import java.time.Instant

class executionTwilight extends ExecutionTestSupportForGmos {

  // Need a timestamp to call the calibrations service
  val when: Instant =
    Instant.ofEpochMilli(1729596890131L)

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  case class Calibrations(
    specPhot: Observation.Id,
    twilight: Observation.Id
  )

  // Picks the expected twilight observation out of the program's observations
  def twilight(pid: Program.Id): IO[Calibrations] =
    query(
      pi,
      s"""
         query {
          observations(WHERE: { program: { id: { EQ: "$pid" } } }) {
            matches {
              id
              calibrationRole
            }
          }
        }
      """
    ).flatMap: json =>
      json.hcursor.downFields("observations", "matches").values.toList.flatten.traverse: json =>
        val c = json.hcursor
        for
          id <- c.downField("id").as[Observation.Id]
          ro <- c.downField("calibrationRole").as[Option[CalibrationRole]]
        yield (id, ro)
      .leftMap(f => new RuntimeException(f.message))
      .map: res =>
        val m = res.groupMap(_._2)(_._1)
        Calibrations(
          m(Some(CalibrationRole.SpectroPhotometric)).head,
          m(Some(CalibrationRole.Twilight)).head
        )
      .liftTo[IO]

  val setupScienceObs: IO[(Program.Id, Observation.Id)] =
    for
      p <- createProgram
      t <- createTargetAs(pi, p, "real target")
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
    yield (p, o)

  val setup: IO[(Program.Id, Observation.Id, Calibrations)] =
    for
      (p, o) <- setupScienceObs
      _ <- withServices(serviceUser) { services =>
        services.session.transaction.use: xa =>
          services
            .calibrationsService
            .recalculateCalibrations(p, when)(using xa)
            .map(_._1)
      }
      c <- twilight(p)
    yield (p, o, c)

  def query(sequenceType: String, oid: Observation.Id): String =
    s"""
       query {
         observation(observationId: "$oid") {
           execution {
             config {
               gmosNorth {
                 $sequenceType {
                   nextAtom {
                     observeClass
                     steps {
                       observeClass
                       instrumentConfig {
                         exposure {
                           seconds
                         }
                         readout {
                           xBin
                           yBin
                           ampCount
                           ampGain
                           ampReadMode
                         }
                         dtax
                         roi
                         gratingConfig {
                           grating
                           order
                           wavelength {
                             nanometers
                           }
                         }
                         filter
                         fpu {
                           builtin
                           customMask { slitWidth }
                         }
                       }
                       telescopeConfig {
                         offset {
                           p { arcseconds }
                           q { arcseconds }
                         }
                       }
                     }
                   }
                   possibleFuture {
                     steps {
                       instrumentConfig {
                         exposure {
                           seconds
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
     """

  test("twilight - science"):
    setup.flatMap { case (_, _, Calibrations(_, oid)) =>
      expect(
        user  = pi,
        query = query("science", oid),
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "science": {
                        "nextAtom": {
                          "observeClass": "DAY_CAL",
                          "steps": [
                            {
                              "observeClass": "DAY_CAL",
                              "instrumentConfig": {
                                "exposure": {
                                  "seconds": 30.000000
                                },
                                "readout": {
                                  "xBin": "ONE",
                                  "yBin": "TWO",
                                  "ampCount": "TWELVE",
                                  "ampGain": "LOW",
                                  "ampReadMode": "SLOW"
                                },
                                "dtax": "ZERO",
                                "roi": "CENTRAL_SPECTRUM",
                                "gratingConfig": {
                                  "grating": "R831_G5302",
                                  "order": "ONE",
                                  "wavelength": {
                                    "nanometers": 500.000
                                  }
                                },
                                "filter": "R_PRIME",
                                "fpu": {
                                  "builtin": "LONG_SLIT_0_50",
                                  "customMask": null
                                }
                              },
                              "telescopeConfig": {
                                "offset": {
                                  "p": {
                                    "arcseconds": 0.000000
                                  },
                                  "q": {
                                    "arcseconds": 0.000000
                                  }
                                }
                              }
                            }
                          ]
                        },
                        "possibleFuture": []
                      }
                    }
                  }
                }
              }
            }
          """.asRight
      )
    }

  test("twilight - acquisition"):
    setup.flatMap { case (_, _, Calibrations(_, oid)) =>
      expect(
        user  = pi,
        query = query("acquisition", oid),
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "acquisition": null
                    }
                  }
                }
              }
            }
          """.asRight
      )
    }

  test("twilight - observation timeEstimate"):
    setup.flatMap { case (pid, _, Calibrations(_, oid)) =>
      runObscalcUpdate(pid, oid) *>
      expect(
        user  = pi,
        query = s"""
          query {
            observation(observationId: "$oid") {
              execution {
                digest {
                  value {
                    science {
                      observeClass
                      timeEstimate {
                        program { seconds }
                        nonCharged { seconds }
                        total { seconds }
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
            "observation": {
              "execution": {
                "digest": {
                  "value": {
                    "science": {
                      "observeClass": "DAY_CAL",
                      "timeEstimate": {
                        "program": { "seconds":  0.000000 },
                        "nonCharged": { "seconds":  50.700000 },
                        "total": { "seconds":  50.700000 }
                      }
                    }
                  }
                }
              }
            }
          }
        """.asRight
      )
    }

  def programTimeEstimate(p: Program.Id): IO[CategorizedTime] =
    query(
      user  = pi,
      query = s"""
        query {
          program(programId: "$p") {
            timeEstimateRange {
              value {
                maximum {
                  program { seconds }
                  nonCharged { seconds }
                  total { seconds }
                }
              }
            }
          }
        }
      """
    ).map: json =>
      json.hcursor.downFields("program", "timeEstimateRange", "value", "maximum").require[CategorizedTime]

  def obsTimeEstimate(oid: Observation.Id): IO[CategorizedTime] =
    query(
      user  = pi,
      query = s"""
        query {
          observation(observationId: "$oid") {
            execution {
              digest {
                value {
                  setup {
                    full {
                      seconds
                    }
                  }
                  science {
                    observeClass
                    timeEstimate {
                      program { seconds }
                      nonCharged { seconds }
                      total { seconds }
                    }
                  }
                }
              }
            }
          }
        }
      """
    ).map: json =>
      val digest   = json.hcursor.downFields("observation", "execution", "digest", "value")
      val setup    = digest.downFields("setup", "full").require[TimeSpan]
      val obsClass = digest.downFields("science", "observeClass").require[ObserveClass]
      val catTime  = digest.downFields("science", "timeEstimate").require[CategorizedTime]
      catTime.sumCharge(obsClass.chargeClass, setup)

  test("twilight - program timeEstimate"):
    assertIOBoolean(for
      (p0, o0)    <- setupScienceObs
      (p1, o1, c) <- setup
      _           <- runObscalcUpdate(p0, o0)
      _           <- runObscalcUpdate(p1, o1)
      _           <- runObscalcUpdate(p1, c.specPhot)

      t0          <- programTimeEstimate(p0)
      t1          <- programTimeEstimate(p1)
      cSpecPhot   <- obsTimeEstimate(c.specPhot)
    yield (t0.programTime +| cSpecPhot.programTime) === t1.programTime)

}
