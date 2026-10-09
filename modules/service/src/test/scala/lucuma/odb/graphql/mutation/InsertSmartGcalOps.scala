// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Step
import munit.Location

trait InsertSmartGcalOps:
  self: OdbSuite =>

  def insertQuery(
    oid:          Observation.Id,
    types:        List[SmartGcalType],
    after:        Option[Step.Id]      = None,
    sequenceType: Option[SequenceType] = None,
    reference:    Option[Step.Id]      = None
  ): String =
    val afterField  = after.foldMap(sid => s"afterStepId: ${sid.asJson}")
    val refField    = reference.foldMap(sid => s"referenceStepId: ${sid.asJson}")
    val seqField    = sequenceType.foldMap(st => s"sequenceType: ${st.tag.toUpperCase}")
    s"""
      mutation {
        insertSmartGcal(input: {
          observationId: ${oid.asJson}
          smartGcalTypes: ${types.map(_.tag.toUpperCase).mkString("[", ", ", "]")}
          $refField
          $afterField
          $seqField
        }) {
          observation { id }
        }
      }
    """

  def insertAs(
    user:         User,
    oid:          Observation.Id,
    types:        List[SmartGcalType],
    after:        Option[Step.Id]      = None,
    sequenceType: Option[SequenceType] = None,
    reference:    Option[Step.Id]      = None
  )(using Location): IO[Unit] =
    expect(
      user,
      insertQuery(oid, types, after, sequenceType, reference),
      json"""
        {
          "insertSmartGcal": {
            "observation": { "id": $oid }
          }
        }
      """.asRight
    )

  /** Inserts the steps, returning the ids reported by the mutation. */
  def insertReturningIdsAs(
    user:  User,
    oid:   Observation.Id,
    types: List[SmartGcalType],
    after: Option[Step.Id] = None
  ): IO[List[Step.Id]] =
    query(user, insertQuery(oid, types, after).replace("observation { id }", "observation { id } stepIds")).flatMap: json =>
      json.hcursor.downField("insertSmartGcal").downField("stepIds").as[List[Step.Id]].liftTo[IO]

  def insertFails(
    user:      User,
    oid:       Observation.Id,
    types:     List[SmartGcalType],
    after:     Option[Step.Id],
    errors:    List[String],
    reference: Option[Step.Id] = None
  )(using Location): IO[Unit] =
    expect(user, insertQuery(oid, types, after, reference = reference), errors.asLeft)
