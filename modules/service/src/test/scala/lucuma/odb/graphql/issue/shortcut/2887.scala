// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupport

class ShortCut_2887 extends ExecutionTestSupport {

  override def fakeItcResult: IntegrationTime =
    IntegrationTime(
      596523.hourTimeSpan,
      PosInt.unsafeFrom(2147483647),
      SignalToNoise.unsafeFromBigDecimalExact(50.0)
    )

  test("forever sequence") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   digest {
                     setup {
                       full { seconds }
                       reacquisition { seconds }
                     }
                     acquisition {
                       observeClass
                       timeEstimate {
                         program { seconds }
                         partner { seconds }
                         nonCharged { seconds }
                         total { seconds }
                       }
                       offsets {
                         p { arcseconds }
                         q { arcseconds }
                       }
                       atomCount
                     }
                     science {
                       observeClass
                       timeEstimate {
                         program { seconds }
                         partner { seconds }
                         nonCharged { seconds }
                         total { seconds }
                       }
                       offsets {
                         p { arcseconds }
                         q { arcseconds }
                       }
                       atomCount
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
                  "digest": {
                    "setup" : {
                      "full" : {
                        "seconds" : 960.000000
                      },
                      "reacquisition" : {
                        "seconds" : 300.000000
                      }
                    },
                    "acquisition" : {
                      "observeClass" : "ACQUISITION",
                      "timeEstimate" : {
                        "program" : {
                          "seconds" : 175.162500
                        },
                        "partner" : {
                          "seconds" : 0.000000
                        },
                        "nonCharged" : {
                          "seconds" : 0.000000
                        },
                        "total" : {
                          "seconds" : 175.162500
                        }
                      },
                      "offsets" : [
                        {
                          "p" : {
                            "arcseconds" : 0.000000
                          },
                          "q" : {
                            "arcseconds" : 0.000000
                          }
                        },
                        {
                          "p" : {
                            "arcseconds" : 10.000000
                          },
                          "q" : {
                            "arcseconds" : 0.000000
                          }
                        }
                      ],
                      "atomCount": 1
                    },
                    "science" : {
                      "observeClass" : "SCIENCE",
                      "timeEstimate" : {
                        "program" : {
                          "seconds" : 411.600000
                        },
                        "partner" : {
                          "seconds" : 357.600000
                        },
                        "nonCharged" : {
                          "seconds" : 0.000000
                        },
                        "total" : {
                          "seconds" : 769.200000
                        }
                      },
                      "offsets" : [
                        {
                          "p" : {
                            "arcseconds" : 0.000000
                          },
                          "q" : {
                            "arcseconds" : 0.000000
                          }
                        },
                        {
                          "p" : {
                            "arcseconds" : 0.000000
                          },
                          "q" : {
                            "arcseconds" : 15.000000
                          }
                        }
                      ],
                      "atomCount": 6
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