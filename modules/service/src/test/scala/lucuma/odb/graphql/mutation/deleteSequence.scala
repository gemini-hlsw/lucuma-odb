// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation

// There is nothing instrument or mode specific about the delete sequence mutation (at least for now), so
// we'll just use GmosNorth for testing it.
class deleteSequence extends query.ExecutionTestSupportForGmos with ReplaceGmosNorthSequenceOps:

  def expectMaterializedSequences(o: Observation.Id, acquistion: Boolean, science: Boolean): IO[Unit] =
    expect(
      pi,
      s"""
        query {
          observation(observationId: ${o.asJson}) {
            execution {
              acquisitionSequenceIsMaterialized
              scienceSequenceIsMaterialized
            }
          }
        }
      """,
      json"""
        {
          "observation": {
            "execution": {
              "acquisitionSequenceIsMaterialized": ${acquistion.asJson},
              "scienceSequenceIsMaterialized": ${science.asJson}
            }
          }
        }
      """.asRight,
    )

  val expectedSuccessResponse: Json =
    json"""
      {
        "deleteSequence": {
          "observation": {
            "execution": {
              "acquisitionSequenceIsMaterialized": false,
              "scienceSequenceIsMaterialized": false
            }
          }
        }
      }
    """

  def deleteSequence(oid: Observation.Id, expected: Either[List[String], Json] = expectedSuccessResponse.asRight): IO[Unit] =
    expect(
      pi,
      s"""
        mutation {
          deleteSequence(input: { observationId: ${oid.asJson} }) {
            observation {
              execution {
                acquisitionSequenceIsMaterialized
                scienceSequenceIsMaterialized
              }
            }
          }
        }
      """,
      expected
    )

  def replaceSequence(oid: Observation.Id, sequenceType: SequenceType): IO[Unit] =
    val inputString = input(oid, sequenceType, atomInput("Foo", stepInput(GmosNorthFilter.GPrime)))
    expect(
      pi,
      s"""
        mutation {
          replaceGmosNorthSequence(input: $inputString) {
            sequence {
              description
              steps {
                instrumentConfig {
                  filter
                }
              }
            }
          }
        }
      """,
      json"""
        {
          "replaceGmosNorthSequence": {
            "sequence": [
              {
                "description": "Foo",
                "steps": [
                  {
                    "instrumentConfig": { "filter": "G_PRIME" }
                  }
                ]
              }
            ]
          }
        }
      """.asRight
    )

  val setup: IO[Observation.Id] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
    yield o

  test("delete unmaterialized sequence is a no-op"):
    for 
      oid <- setup
      _   <- expectMaterializedSequences(oid, acquistion = false, science = false)
      _   <- deleteSequence(oid)
    yield ()

  test("delete with modified acquisition sequence is succesful"):
    for
      oid <- setup
      _   <- expectMaterializedSequences(oid, acquistion = false, science = false)
      _   <- replaceSequence(oid, SequenceType.Acquisition)
      _   <- expectMaterializedSequences(oid, acquistion = true, science = false)
      _   <- deleteSequence(oid)
      _   <- expectMaterializedSequences(oid, acquistion = false, science = false)
    yield ()

  test("delete with both modified acquisition and science sequences is succesful"):
    for
      oid <- setup
      _   <- expectMaterializedSequences(oid, acquistion = false, science = false)
      _   <- replaceSequence(oid, SequenceType.Acquisition)
      _   <- replaceSequence(oid, SequenceType.Science)
      _   <- expectMaterializedSequences(oid, acquistion = true, science = true)
      _   <- deleteSequence(oid)
      _   <- expectMaterializedSequences(oid, acquistion = false, science = false)
    yield ()

  test("delete with modified science sequence is succesful"):
    for
      oid <- setup
      _   <- expectMaterializedSequences(oid, acquistion = false, science = false)
      _   <- replaceSequence(oid, SequenceType.Science)
      _   <- expectMaterializedSequences(oid, acquistion = false, science = true)
      _   <- deleteSequence(oid)
      _   <- expectMaterializedSequences(oid, acquistion = false, science = false)
    yield ()

  test("attempting to delete with a visit is an error"):
    for
      oid <- setup
      _   <- expectMaterializedSequences(oid, acquistion = false, science = false)
      _   <- recordVisitAs(serviceUser, oid)
      _   <- expectMaterializedSequences(oid, acquistion = true, science = true)
      _   <- deleteSequence(oid, List(s"Cannot delete sequence for observation $oid because it has visits.").asLeft)
    yield ()

  test("attempting to delete after execution is an error"):
    for
      oid <- setup
      _   <- expectMaterializedSequences(oid, acquistion = false, science = false)
      v   <- recordVisitAs(serviceUser, oid)
      s   <- firstAcquisitionStepId(serviceUser, oid)
      _   <- addEndStepEvent(s, v)
      _   <- expectMaterializedSequences(oid, acquistion = true, science = true)
      _   <- deleteSequence(
              oid,
              List(
                s"Observation $oid is ineligible for this operation due to its workflow state (Ongoing with allowed transition to Inactive/Completed).",
                "User cannot delete the sequence in the current observation workflow state."
              ).asLeft)
    yield ()
