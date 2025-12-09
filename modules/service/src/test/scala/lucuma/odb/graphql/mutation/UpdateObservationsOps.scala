// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.User

trait UpdateObservationsOps { this: OdbSuite =>

  def updateObservationsMutation(
    oid:    Observation.Id,
    update: String,
    query:  String
  ): String = s"""
    mutation {
      updateObservations(input: {
        SET: {
          $update
        },
        WHERE: {
          id: { EQ: ${oid.asJson} }
        }
      }) {
        $query
      }
    }
  """

  def updateObservation(
    user:     User,
    oid:      Observation.Id,
    update:   String,
    query:    String,
    expected: Either[String, Json]
  ): IO[Unit] =
    expect(
      user     = user,
      query    = updateObservationsMutation(oid, update, query),
      expected = expected.leftMap(msg => List(msg))
    )

  def updateObservationsTimesMutation(
    oid:    Observation.Id,
    update: String,
    query:  String
  ): String = s"""
    mutation {
      updateObservationsTimes(input: {
        SET: {
          $update
        },
        WHERE: {
          id: { EQ: ${oid.asJson} }
        }
      }) {
        $query
      }
    }
  """

  def updateObservationTimes(
    user:     User,
    oid:      Observation.Id,
    update:   String,
    query:    String,
    expected: Either[String, Json]
  ): IO[Unit] =
    expect(
      user     = user,
      query    = updateObservationsTimesMutation(oid, update, query),
      expected = expected.leftMap(msg => List(msg))
    )

  protected def oneUpdateTest(
    user:          User,
    update:        String,
    query:         String,
    expected:      Either[String, Json],
    observingMode: Option[ObservingModeType] = None
  ): IO[Unit] =

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, observingMode)
      _   <- updateObservation(user, oid, update, query, expected)
    yield ()

  protected def multiUpdateTest(
    user:    User,
    updates: List[(String, String, Either[String, Json])],
    observingMode: Option[ObservingModeType] = None
  ): IO[Unit] =

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, observingMode)
      _   <- updates.traverse_ { case (update, query, expected) =>
        updateObservation(user, oid, update, query, expected)
      }
    yield ()

  protected def multiUpdateTestWithOid(
    user:    User,
    updates: List[(String, String, Observation.Id => Either[String, Json])],
    observingMode: Option[ObservingModeType] = None
  ): IO[Unit] =

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, observingMode)
      _   <- updates.traverse_ { case (update, query, expected) =>
        updateObservation(user, oid, update, query, expected(oid))
      }
    yield ()
}
