// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.github

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Instrument
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.odb.graphql.query.ExecutionQuerySetupOperations
import lucuma.odb.data.ObservingModeType


// Not nullable at List(continuum, stepConfig, matches, steps, matches, atomRecords, matches, visits, execution, observation)
class GitHub_1060 extends OdbSuite with ExecutionQuerySetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val service = TestUsers.service(3)
  val mode    = ObservingModeType.GmosNorthLongSlit

  val validUsers = List(pi, service).toList

  override def recordStepAs(user: User, instrument: Instrument, aid: Atom.Id): IO[Step.Id] =
    recordStepAs(user, aid, instrument, dynamicConfig(instrument), stepConfigGcal)

  val stepConfigGcal: String =
    """
      stepConfig: {
        gcal: {
          continuum: QUARTZ_HALOGEN100
          diffuser: IR
          filter: GMOS
          shutter: OPEN
        }
      }
    """

  test("GitHub_1060") {
    recordAll(pi, service, mode, offset = 0, datasetCount = 2).flatMap { on =>
      expect(pi, s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              atomRecords {
                matches {
                  steps {
                    matches {
                      stepConfig {
                        stepType
                        ... on Gcal {
                          continuum
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
      json"""
        {
          "observation" : {
            "execution" : {
              "atomRecords" : {
                "matches" : [
                  {
                    "steps" : {
                      "matches" : [
                        {
                          "stepConfig" : {
                            "stepType" : "GCAL",
                            "continuum" : "QUARTZ_HALOGEN100"
                          }
                        }
                      ]
                    }
                  }
                ]
              }
            }
          }
        }
      """.asRight
      )
    }
  }
}