// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.User

class updateObservationsTimes extends OdbSuite
                            with UpdateObservationsOps {

  val pi: User    = TestUsers.Standard.pi(nextId, nextId)
  val pi2: User   = TestUsers.Standard.pi(nextId, nextId)
  val staff: User = TestUsers.Standard.staff(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi, pi2, staff)

  private val Query = """
    observations {
      observationTime
      observationDuration { microseconds }
    }
  """

  private def expected(time: Option[String], seconds: Option[Int]): Either[Nothing, Json] = 
    val expectedDuration = seconds.fold(Json.Null)(s => Json.obj("microseconds" -> (s * 1000000).asJson))
    json"""
      {
        "updateObservationsTimes": {
          "observations": [
            {
              "observationTime": $time,
              "observationDuration": $expectedDuration
            }
          ]
        }
      }
    """.asRight

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

  test("observation time: set ISO-8601") {

    val timeStr = "2022-08-30 17:18:00"
    val update = s"""
      observationTime: "$timeStr"
    """

    oneUpdateTest(pi, update, Query, expected(timeStr.some, none))
  }

  test("observation duration: set") {

    val update = s"""
      observationDuration: { seconds: 10 }
    """

    oneUpdateTest(pi, update, Query, expected(none, 10.some))
  }
  
  test("observation time and duration: set") {

    val timeStr = "2022-08-30 17:18:00"
    val update = s"""
      observationTime: "$timeStr", observationDuration: { seconds: 12 }
    """

    oneUpdateTest(pi, update, Query, expected(timeStr.some, 12.some))
  }
  test("observation time: delete") {

    val timeStr = "2022-08-30 17:18:00"
    val update0 = s"""
      observationTime: "$timeStr", observationDuration: { seconds: 13 }
    """

    val update1 = """
      observationTime: null
    """

    val expected0 = expected(timeStr.some, 13.some)
    val expected1 = expected(none, 13.some)

    multiUpdateTest(pi, List((update0, Query, expected0), (update1, Query, expected1)))
  }

  test("observation duration: delete") {

    val timeStr = "2022-08-30 17:18:00"
    val update0 = s"""
      observationTime: "$timeStr", observationDuration: { seconds: 10 }
    """

    val update1 = """
      observationDuration: null
    """

    val expected0 = expected(timeStr.some, 10.some)
    val expected1 = expected(timeStr.some, none)

    multiUpdateTest(pi, List((update0, Query, expected0), (update1, Query, expected1)))
  }
  
  test("observation time and duration: delete") {

    val timeStr = "2022-08-30 17:18:00"
    val update0 = s"""
      observationTime: "$timeStr", observationDuration: { seconds: 10 }
    """

    val update1 = """
      observationDuration: null, observationTime: null
    """

    val expected0 = expected(timeStr.some, 10.some)
    val expected1 = expected(none, none)

    multiUpdateTest(pi, List((update0, Query, expected0), (update1, Query, expected1)))
  }
}
