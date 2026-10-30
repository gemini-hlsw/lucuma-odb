// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.subscription

import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.mutation.DaytimeCalibrationOps

class observationEditOnCalibrationSets extends OdbSuite with DaytimeCalibrationOps with SubscriptionUtils:

  val staff = TestUsers.Standard.staff(3, 103)

  override val validUsers = List(staff)

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

  def replaceSets(user: User, oid: Observation.Id) =
    sleep >>
      query(
        user  = user,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                observingMode: {
                  daytimeCalibration: {
                    calibrationSets: [
                      {
                        kind: DARK
                        explicitCount: 2
                        gmosNorth: {
                          exposure: { seconds: 10 }
                          readout: { xBin: TWO, yBin: TWO, ampCount: TWELVE, ampGain: LOW, ampReadMode: SLOW }
                          dtax: ZERO
                          roi: FULL_FRAME
                        }
                      }
                    ]
                  }
                }
              }
              WHERE: { id: { EQ: "$oid" } }
            }) {
              observations { id }
            }
          }
        """
      )

  test("triggers when the calibration sets change"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid)
      _   <- subscriptionExpect(
        user      = staff,
        query     = observationUpdateSubscription(oid),
        mutations = Right(replaceSets(staff, oid)),
        expected  = List(updateResponse)
      )
    yield ()
