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
import lucuma.odb.util.Codecs.*
import munit.Location
import skunk.*
import skunk.codec.numeric.int8
import skunk.implicits.*

// There is nothing instrument or mode specific about the delete sequence mutation (at least for now), so
// we'll just use GmosNorth for testing it.
class deleteSequence extends query.ExecutionTestSupportForGmos with ReplaceGmosNorthSequenceOps:

  def expectMaterializedSequences(o: Observation.Id, acquistion: Boolean, science: Boolean)(using Location): IO[Unit] =
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

  def deleteSequence(
    oid: Observation.Id,
    expected: Either[List[String], Json] = expectedSuccessResponse.asRight
  )(using Location): IO[Unit] =
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

  def replaceSequence(oid: Observation.Id, sequenceType: SequenceType)(using Location): IO[Unit] =
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

  def expectAtomCount(oid: Observation.Id, expected: Long)(using Location): IO[Unit] =
    withSession: session =>
      val query = sql"""
        SELECT COUNT(*)
        FROM t_atom
        WHERE c_observation_id = $observation_id
      """.query(int8)
      session.unique(query)(oid)
    .map: count =>
      assertEquals(count, expected)

  def setup(using Location): IO[Observation.Id] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _ <- expectMaterializedSequences(o, acquistion = false, science = false)
      _ <- expectAtomCount(o, 0)
    yield o

  test("delete unmaterialized sequence is a no-op"):
    for 
      oid <- setup
      _   <- deleteSequence(oid)
      _   <- expectMaterializedSequences(oid, acquistion = false, science = false)
      _   <- expectAtomCount(oid, 0)
    yield ()

  test("delete with modified acquisition sequence is succesful"):
    for
      oid <- setup
      _   <- replaceSequence(oid, SequenceType.Acquisition)
      _   <- expectMaterializedSequences(oid, acquistion = true, science = false)
      _   <- expectAtomCount(oid, 1)
      _   <- deleteSequence(oid)
      _   <- expectMaterializedSequences(oid, acquistion = false, science = false)
      _   <- expectAtomCount(oid, 0)
    yield ()

  test("delete with modified science sequence is succesful"):
    for
      oid <- setup
      _   <- replaceSequence(oid, SequenceType.Science)
      _   <- expectMaterializedSequences(oid, acquistion = false, science = true)
      _   <- expectAtomCount(oid, 1)
      _   <- deleteSequence(oid)
      _   <- expectMaterializedSequences(oid, acquistion = false, science = false)
      _   <- expectAtomCount(oid, 0)
    yield ()

  test("delete with both modified acquisition and science sequences is succesful"):
    for
      oid <- setup
      _   <- replaceSequence(oid, SequenceType.Acquisition)
      _   <- replaceSequence(oid, SequenceType.Science)
      _   <- expectMaterializedSequences(oid, acquistion = true, science = true)
      _   <- expectAtomCount(oid, 2)
      _   <- deleteSequence(oid)
      _   <- expectMaterializedSequences(oid, acquistion = false, science = false)
      _   <- expectAtomCount(oid, 0)
    yield ()

  test("attempting to delete with a visit is an error"):
    for
      oid <- setup
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
