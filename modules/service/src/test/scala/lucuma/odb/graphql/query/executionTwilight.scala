// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime

import java.time.Instant

class executionTwilight extends ExecutionTestSupport {

  // Need a timestamp to call the calibrations service
  val when: Instant =
    Instant.ofEpochMilli(1729596890131L)

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10),
      SignalToNoise.unsafeFromBigDecimalExact(50.0)
    )

  // Picks the expected twilight observation out of the program's observations
  def twilight(pid: Program.Id): IO[Observation.Id] =
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
      .map(_.collect { case (id, Some(CalibrationRole.Twilight)) => id }.head)
      .liftTo[IO]

  val setup: IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetAs(pi, p, "real target")
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _ <- withServices(staff) { services =>
        services.session.transaction.use: xa =>
          services
            .calibrationsService
            .recalculateCalibrations(p, when)(using xa)
            .map(_._1)
      }
      oʹ <- twilight(p)
    yield oʹ

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
                       stepConfig {
                         ... on Science {
                           offset {
                             p { arcseconds }
                             q { arcseconds }
                           }
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

  test("twilight - science") {
    setup.flatMap: oid =>
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
                          "observeClass": "SCIENCE",
                          "steps": [
                            {
                              "observeClass": "SCIENCE",
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
                              "stepConfig": {
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

  test("twilight - acquisition") {
    setup.flatMap: oid =>
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

}
