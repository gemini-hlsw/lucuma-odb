// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.subscription

import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

class observationEditOnSequenceReplace extends ExecutionTestSupportForGmos with SubscriptionUtils:

  def observationUpdateSubscription(oid: Observation.Id): String =
    s"""
      subscription {
        observationEdit(input: { observationId: "$oid" }) {
          editType
        }
      }
    """

  val updateResponse: Json =
    json"""
      {
        "observationEdit": {
          "editType": "UPDATED"
        }
      }
    """

  def replaceSequence(user: User, oid: Observation.Id) =
    sleep >>
      query(
        user  = user,
        query = s"""
          mutation {
            replaceGmosNorthSequence(input: {
              observationId: "$oid"
              sequenceType: SCIENCE,
              sequence: [
                {
                  description: "Foo"
                  steps: [
                    {
                      instrumentConfig: {
                        exposure: {
                          seconds: 20
                        }
                        readout: {
                          xBin: ONE
                          yBin: ONE
                          ampCount: TWELVE
                          ampGain: LOW
                          ampReadMode: SLOW
                        }
                        dtax: ZERO
                        roi: FULL_FRAME
                        gratingConfig: {
                          grating: R831_G5302
                          order: ZERO
                          wavelength: {
                            nanometers: 500.0
                          }
                        }
                        filter: Z
                        fpu: {
                          builtin: LONG_SLIT_0_50
                        }
                      }
                      stepConfig: {
                        science: true
                      }
                      observeClass: SCIENCE
                    }
                  ]
                }
              ]
            }) {
              sequence { description }
            }
          }
        """
      )

  test("triggers when replacing a sequence"):
    for
      pid <- createProgram(pi, "foo")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))

      _   <- subscriptionExpect(
        user      = pi,
        query     = observationUpdateSubscription(oid),
        mutations = Right(replaceSequence(pi, oid)),
        expected  = List(updateResponse)
      )
    yield ()