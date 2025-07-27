// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.graphql.query.ObservingModeSetupOperations

class ShortCut_2772 extends ExecutionTestSupportForGmos {

  // specify 2 exposures per atom (30 min x 2 fills a 1 hour block)
  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      30.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  val user: User = serviceUser

  // R831 grating with a 1" slit and IQ=1.0"

  def createTargetWithProfile(pid: Program.Id): IO[Target.Id] =
    query(
      user  = user,
      query =
      s"""
         mutation {
           createTarget(input: {
             programId: ${pid.asJson},
             SET: {
               name: "V1647 Orionis"
               sidereal: {
                 ra: { hms: "05:46:13.137" },
                 dec: { dms: "-00:06:04.89" },
                 epoch: "J2000.0",
                 properMotion: {
                   ra: {
                     milliarcsecondsPerYear: 0.918
                   },
                   dec: {
                     milliarcsecondsPerYear: -1.057
                   },
                 },
                 radialVelocity: {
                   kilometersPerSecond: 27.58
                 },
                 parallax: {
                   milliarcseconds: 2.422
                 }
               },
               sourceProfile: {
                 point: {
                   bandNormalized: {
                     sed: {
                       stellarLibrary: O5_V
                     },
                     brightnesses: [
                       {
                         band: J,
                         value: 14.74,
                         units: VEGA_MAGNITUDE
                       },
                       {
                         band: V,
                         value: 18.1,
                         units: VEGA_MAGNITUDE
                       }
                     ]
                   }
                 }
               }
             }
           }) {
             target {
               id
             }
           }
         }
      """
    ).map(
      _.hcursor.downFields("createTarget", "target", "id").require[Target.Id]
    )

  def createGmosNorthLongSlitObservation(
    pid: Program.Id,
    tid: Target.Id,
  ): IO[Observation.Id] =
    query(
      user  = user,
      query =
      s"""
         mutation {
           createObservation(input: {
             programId: ${pid.asJson},
             SET: {
               constraintSet: {
                 cloudExtinction: POINT_ONE,
                 imageQuality: ONE_POINT_ZERO,
                 skyBackground: DARKEST
               },
               targetEnvironment: {
                 asterism: ${List(tid).asJson}
               },
               ${ObservingModeSetupOperations.SpectroscopyScienceRequirements},
               observingMode: {
                 gmosNorthLongSlit: {
                   grating: R831_G5302,
                   fpu: LONG_SLIT_1_00,
                   centralWavelength: {
                     nanometers: 500
                   }
                 }
               }
             }
           }) {
             observation {
               id
             }
           }
         }
      """
    ).map { json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  test("binning calculation is correct") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfile(p)
        o <- createGmosNorthLongSlitObservation(p, t)
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       science {
                         nextAtom {
                           steps {
                             stepConfig {
                               stepType
                             }
                             instrumentConfig {
                               readout {
                                 xBin
                                 yBin
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
           """,
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "science": {
                        "nextAtom": {
                          "steps": [
                            {
                              "stepConfig": {
                                "stepType": "GCAL"
                              },
                              "instrumentConfig": {
                                "readout": {
                                  "xBin": "TWO",
                                  "yBin": "FOUR"
                                }
                              }
                            },
                            {
                              "stepConfig": {
                                "stepType": "GCAL"
                              },
                              "instrumentConfig": {
                                "readout": {
                                  "xBin": "TWO",
                                  "yBin": "FOUR"
                                }
                              }
                            },
                            {
                              "stepConfig": {
                                "stepType": "SCIENCE"
                              },
                              "instrumentConfig": {
                                "readout": {
                                  "xBin": "TWO",
                                  "yBin": "FOUR"
                                }
                              }
                            },
                            {
                              "stepConfig": {
                                "stepType": "SCIENCE"
                              },
                              "instrumentConfig": {
                                "readout": {
                                  "xBin": "TWO",
                                  "yBin": "FOUR"
                                }
                              }
                            }
                          ]
                        }
                      }
                    }
                  }
                }
              }
            }
          """
        )
      )
    }

  }

}
