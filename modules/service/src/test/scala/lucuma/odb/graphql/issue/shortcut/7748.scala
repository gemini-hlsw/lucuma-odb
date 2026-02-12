// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package issue.shortcut

import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Observation
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.mutation.UpdateObservationsOps
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

class ShortCut_7748 extends ExecutionTestSupportForGmos with UpdateObservationsOps:
  private def blindOffsetMutation(oid: Observation.Id): String =
    s"""
      mutation {
        updateObservations(input: {
          SET: {
            targetEnvironment: {
              useBlindOffset: true
              blindOffsetTarget: null
              blindOffsetType: AUTOMATIC
            }
          }
          WHERE: { id: { EQ: "${oid}" } }
        }) {
          observations { id }
        }
      }
    """
 
  test("pi cannot update blindoffset for ongoing observation"):
      createOngoingGmosNorthObservation.flatMap: oid =>
        expectOdbError(
          user = pi,
          query = blindOffsetMutation(oid),
          expected = {
            case OdbError.InvalidObservation(_, Some(msg)) if msg.contains("is ineligible for this operation due to its workflow state (Ongoing with allowed transition to Inactive/Completed)") => ()
          }
        )
  
  test("staff can update blindoffset for ongoing observation"):
      createOngoingGmosNorthObservation.flatMap: oid =>
        updateObservation(
          user = staff,
          oid,
          update = s"""
            targetEnvironment: {
              useBlindOffset: true
              blindOffsetTarget: null
              blindOffsetType: AUTOMATIC
            }
          """,
          query = s"""
                observations {
                  targetEnvironment {
                    useBlindOffset
                    blindOffsetTarget { name }
                    blindOffsetType
                  }
                }
          """,
          expected = json"""
            {
              "updateObservations": {
                "observations": [
                  {
                    "targetEnvironment": {
                      "useBlindOffset": true,
                      "blindOffsetTarget": null,
                      "blindOffsetType": "AUTOMATIC"
                    }
                  }
                ]
              }
            }
          """.asRight
        )
  
  test("staff cannot update explicit base coordinates for ongoing observation"):
      createOngoingGmosNorthObservation.flatMap: oid =>
        expectOdbError(
          user = staff,
          query = s"""
            mutation {
              updateObservations(input: {
                SET: {
                  targetEnvironment: {
                    explicitBase: {
                      ra: { microseconds: 0 }
                      dec: { microarcseconds: 0 }
                    }
                    useBlindOffset: true
                    blindOffsetTarget: null
                    blindOffsetType: AUTOMATIC
                  }
                }
                WHERE: { id: { EQ: "${oid}" } }
              }) {
                observations { id }
              }
            }
          """,
          expected = {
            case OdbError.InvalidObservation(_, Some(msg)) if msg.contains("is ineligible for this operation due to its workflow state (Ongoing with allowed transition to Inactive/Completed)") => ()
          }
        )
  
  test("staff cannot update blindoffset for completed observation"):
    for
      oid <- createOngoingGmosNorthObservation
      _   <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Completed)
      _   <-  expectOdbError(
          user = staff,
          query = blindOffsetMutation(oid),
          expected = {
            case OdbError.InvalidObservation(_, Some(msg)) if msg.contains("is ineligible for this operation due to its workflow state (Completed with allowed transition to Ongoing)") => ()
          }
        )
    yield ()
  
