// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.ags.GuideStarName
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Timestamp

class setGuideTargetName extends query.ExecutionTestSupport {
  val targetName1: String = GuideStarName.gaiaSourceId.reverseGet(1L).value.value

  val Now: Timestamp = Timestamp.FromString.getOption("2024-08-25T00:00:00Z").get
  val Later: Timestamp = Now.plusSecondsOption(120L).get

  private val targetEnvQuery = """
    targetEnvironment {
      guideEnvironment(lookupIfUndefined: false) {
        guideTargets { name }
      }
    }
  """

  private val idQuery = "id"

  // If name is None, omit from mutation, if the string is empty, set to null, else set name
  private def mutation(
    pid: Program.Id,
    oid: Observation.Id,
    name: Option[String],
    resultQuery: String = targetEnvQuery
  ): String =
    val update = name.fold(""){ n =>
      val nn = if (n.isEmpty) "null" else s"\"$n\""
      s"targetName: $nn"
    }

    s"""
      mutation {
        setGuideTargetName(
          input: {
            programId: ${pid.asJson}
            observationId: ${oid.asJson}
            $update
          }
        ) {
          observation {
            $resultQuery
          }
        }
      }
    """

  private def justQuery(user: User, oid: Observation.Id, expectedName: Option[String]): IO[Unit] =
    val guideEnv = expectedName.fold(Json.Null)(name =>
      Json.obj("guideTargets" ->
        Json.arr(Json.obj("name" -> name.asJson))
      )
    )
    val expected = json"""
      {
        "observation": {
          "targetEnvironment": {
            "guideEnvironment": $guideEnv
          }
        }
      }
    """.asRight
    val query = s"""
      query {
        observation(observationId: ${oid.asJson}) {
          $targetEnvQuery
        }
      }
    """
    expect(user, query, expected)

  private def expectName(guideStarName: String): Either[Nothing, Json] =
    json"""
      {
        "setGuideTargetName": {
          "observation": {
            "targetEnvironment": {
              "guideEnvironment": {
                "guideTargets": [
                  {
                    "name": $guideStarName
                  }
                ]
              }
            }
          }
        }
      }
    """.asRight
  
  private val expectNull: Either[Nothing, Json] =
    json"""
      {
        "setGuideTargetName": {
          "observation": {
            "targetEnvironment": {
              "guideEnvironment": null
            }
          }
        }
      }
    """.asRight

  // for updates where the result query itself would cause an error
  private def expectJustId(oid: Observation.Id): Either[Nothing, Json] =
    json"""
      {
        "setGuideTargetName": {
          "observation": {
            "id": $oid
          }
        }
      }
    """.asRight

  private def expectError(errors: String*): Either[List[String], Json] =
    errors.toList.asLeft

  private def expectObsError(errors: (Observation.Id => String)*): Observation.Id => Either[List[String], Json] =
    obsId => errors.map(_(obsId)).toList.asLeft

  private def setName(
    user: User,
    pid: Program.Id,
    oid: Observation.Id,
    name: Option[String],
    expected: Either[List[String], Json],
    resultQuery: String = targetEnvQuery
  ): IO[Unit] =
    expect(
      user = user,
      query = mutation(pid, oid, name, resultQuery),
      expected = expected
    )

  test("invalid name") {
    val name = targetName1 + "x"
    val expected = expectError(s"Invalid guide target name '$name'")
    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- setName(staff, pid, oid, name.some, expected)
    yield ()
  }

  test("missing target") {
    val expected = expectObsError(oid => s"Could not generate a sequence from the observation $oid: target")
    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- setName(staff, pid, oid, targetName1.some, expected(oid))
    yield ()
  }

  test("missing observing mode") {
    val expected = expectObsError(oid => s"Could not generate a sequence from the observation $oid: observing mode")
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationAs(pi, pid, tid)
      _   <- setName(staff, pid, oid, targetName1.some, expected(oid))
    yield ()
  }

  test("missing observation time") {
    val expected = expectObsError(oid => s"Observation time not set for observation $oid.")
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setName(staff, pid, oid, targetName1.some, expected(oid))
    yield ()
  }

  test("valid set as staff") {
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setObservationTimeAndDuration(pi, oid, Now.some, none)
      _   <- setName(staff, pid, oid, targetName1.some, expectName(targetName1))
    yield ()
  }

  test("valid set as pi") {
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setObservationTimeAndDuration(pi, oid, Now.some, none)
      _   <- setName(pi, pid, oid, targetName1.some, expectName(targetName1))
    yield ()
  }

  test("omission unsets") {
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setObservationTimeAndDuration(pi, oid, Now.some, none)
      _   <- setName(staff, pid, oid, targetName1.some, expectName(targetName1))
      _   <- setName(pi, pid, oid, none, expectNull)
    yield ()
  }

  test("null unsets") {
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setObservationTimeAndDuration(pi, oid, Now.some, none)
      _   <- setName(staff, pid, oid, targetName1.some, expectName(targetName1))
      _   <- setName(pi, pid, oid, "".some, expectNull)
    yield ()
  }

  test("starts as null") {
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setObservationTimeAndDuration(pi, oid, Now.some, none)
      _   <- justQuery(pi, oid, none)
    yield ()
  }

  test("changing observation time resets") {
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setObservationTimeAndDuration(pi, oid, Now.some, none)
      _   <- setName(staff, pid, oid, targetName1.some, expectName(targetName1))
      _   <- setObservationTimeAndDuration(pi, oid, Later.some, none)
      _   <- justQuery(pi, oid, none)
    yield ()
  }

  test("Can always unset") {
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setObservationTimeAndDuration(pi, oid, Now.some, none)
      _   <- setName(staff, pid, oid, targetName1.some, expectName(targetName1))
      _   <- updateAsterisms(pi, List(oid), List.empty, List(tid), List(oid -> List.empty))
      // only query the id in the response since asking for the target name would give an error
      _   <- setName(staff, pid, oid, none, expectJustId(oid), idQuery)
    yield ()
  }

}
