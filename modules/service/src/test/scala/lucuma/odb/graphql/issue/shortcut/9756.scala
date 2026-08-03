// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program

// https://app.shortcut.com/lucuma/story/9756
//
// A fragment reachable by more than one path (here TelescopeConfig, spread both
// directly by Igrins2LongSlit and indirectly via SlitTelescopeConfigs) was
// rejected with "Fragment cycle starting from 'Igrins2LongSlit'".
class ShortCut_9756 extends OdbSuite:

  val pi         = TestUsers.Standard.pi(nextId, nextId)
  val validUsers = List(pi)

  private def createObservation(pid: Program.Id): IO[Observation.Id] =
    query(
      pi,
      s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              scienceRequirements: {
                exposureTimeMode: {
                  signalToNoise: { value: 100.0, at: { nanometers: 2200 } }
                }
              }
              observingMode: {
                igrins2LongSlit: {
                  exposureTimeMode: {
                    signalToNoise: { value: 50.0, at: { nanometers: 2200 } }
                  }
                  svc: {}
                  explicitTelescopeConfigs: {
                    toSky: [
                      { offset: { p: { arcseconds: 1.0 }, q: { arcseconds: 2.0 } } }
                    ]
                  }
                }
              }
            }
          }) {
            observation { id }
          }
        }
      """
    ).map: js =>
      js.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]

  private def diamondFragmentQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          observingMode {
            igrins2LongSlit {
              ...Igrins2LongSlit
            }
          }
        }
      }

      fragment Igrins2LongSlit on Igrins2LongSlit {
        svc {
          telescopeConfigs {
            ...TelescopeConfig
          }
        }
        telescopeConfigs {
          ...SlitTelescopeConfigs
        }
      }

      fragment SlitTelescopeConfigs on SlitTelescopeConfigs {
        toSky {
          ...TelescopeConfig
        }
      }

      fragment TelescopeConfig on TelescopeConfig {
        offset {
          p { arcseconds }
        }
      }
    """

  test("a fragment spread via two paths is not a cycle"):
    val expected: Json =
      json"""
        {
          "svc": {
            "telescopeConfigs": [
              { "offset": { "p": { "arcseconds": 0.000000 } } },
              { "offset": { "p": { "arcseconds": 5.000000 } } }
            ]
          },
          "telescopeConfigs": {
            "toSky": [
              { "offset": { "p": { "arcseconds": 1.000000 } } }
            ]
          }
        }
      """

    val result =
      for
        p <- createProgramAs(pi)
        o <- createObservation(p)
        j <- query(pi, diamondFragmentQuery(o))
      yield j.hcursor.downFields("observation", "observingMode", "igrins2LongSlit").require[Json]

    assertIO(result, expected)
