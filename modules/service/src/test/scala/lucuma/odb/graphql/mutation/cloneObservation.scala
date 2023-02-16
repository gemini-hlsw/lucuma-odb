// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation

class cloneObservation extends OdbSuite {
  import createTarget.FullTargetGraph

  val pi, pi2 = TestUsers.Standard.pi(nextId, nextId)
  lazy val validUsers = List(pi, pi2)

  val FullObservationGraph = s"""
    { 
      existence
      title
      subtitle
      status
      activeStatus
    # visualizationTime
    # posAngleConstraint
    # plannedTime
      program { id }
    # targetEnvironment { asterism { id } }
    # constraintSet
    # scienceRequirements
    # observingMode
    # manualConfig
    # execution
    }
  """

  test("simple clone") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "Vega").flatMap { tid =>
        createObservationAs(pi, pid, tid).flatMap { oid =>
          query(
            user = pi,
            query = s"""
              mutation {
                cloneObservation(input: {
                  observationId: "$oid"
                }) {
                  originalObservation $FullObservationGraph
                  newObservation $FullObservationGraph

                  originalObservationId: originalObservation { id }
                  newObservationId: newObservation { id }
                }
              }
            """
          ).map { json =>

            // The data fields (i.e., everything but ID) should be the same
            assertEquals(
              json.hcursor.downFields("cloneObservation", "originalObservation").as[Json],
              json.hcursor.downFields("cloneObservation", "newObservation").as[Json]
            )

            // The ids should be different
            assertNotEquals(
              json.hcursor.downFields("cloneObservation", "originalObservationId", "Id").as[Observation.Id],
              json.hcursor.downFields("cloneObservation", "newObservationId", "Id").as[Observation.Id]
            )
          
          }
        }
      }
    }
  }

}