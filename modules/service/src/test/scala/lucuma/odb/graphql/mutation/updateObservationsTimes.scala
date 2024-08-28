// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.User

class updateObservationsTimes extends OdbSuite
                            with UpdateConstraintSetOps {

  val pi: User    = TestUsers.Standard.pi(nextId, nextId)
  val pi2: User   = TestUsers.Standard.pi(nextId, nextId)
  val staff: User = TestUsers.Standard.staff(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi, pi2, staff)

  private def oneUpdateTest(
    user:     User,
    update:   String,
    query:    String,
    expected: Either[String, Json]
  ): IO[Unit] =

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid)
      _   <- updateObservationTimes(user, oid, update, query, expected)
    yield ()

  private def multiUpdateTest(
    user:    User,
    updates: List[(String, String, Either[String, Json])]
  ): IO[Unit] =

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid)
      _   <- updates.traverse_ { case (update, query, expected) =>
        updateObservationTimes(user, oid, update, query, expected)
      }
    yield ()

  test("observation time: set") {

    val update   = """
      observationTime: "2022-08-30 17:18:00"
    """

    val query    = "observations { observationTime }"

    val expected = json"""
      {
        "updateObservationsTimes": {
          "observations": [
            {
              "observationTime": "2022-08-30 17:18:00"
            }
          ]
        }
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)
  }

  test("observation time: set ISO-8601") {

    val update = """
      observationTime: "2022-08-30 17:18:00"
    """

    val query = "observations { observationTime }"

    val expected = json"""
      {
        "updateObservationsTimes": {
          "observations": [
            {
              "observationTime": "2022-08-30 17:18:00"
            }
          ]
        }
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)
  }

  test("observation time: delete") {

    val update0  = """
      observationTime: "2022-08-30 17:18:00"
    """

    val update1 = """
      observationTime: null
    """

    val query    = "observations { observationTime }"

    val expected0 = json"""
      {
        "updateObservationsTimes": {
          "observations": [
            {
              "observationTime": "2022-08-30 17:18:00"
            }
          ]
        }
      }
    """.asRight

    val expected1 = json"""
      {
        "updateObservationsTimes": {
          "observations": [
            {
              "observationTime": null
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi, List((update0, query, expected0), (update1, query, expected1)))

  }


}
