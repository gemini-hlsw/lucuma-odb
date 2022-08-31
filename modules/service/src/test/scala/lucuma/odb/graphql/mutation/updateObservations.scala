// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.graphql.input.CoordinatesInput
import lucuma.odb.service.ObservationService

class updateObservations extends OdbSuite
                            with CreateProgramOps
                            with CreateObservationOps
                            with UpdateConstraintSetOps {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  private def oneUpdateTest(
    user:     User,
    update:   String,
    query:    String,
    expected: Either[String, Json]
  ): IO[Unit] =

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid)
      _   <- updateObservation(user, pid, oid, update, query, expected)
    yield ()

  private def multiUpdateTest(
    user:    User,
    updates: List[(String, String, Either[String, Json])]
  ): IO[Unit] =

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid)
      _   <- updates.traverse_ { case (update, query, expected) =>
        updateObservation(user, pid, oid, update, query, expected)
      }
    yield ()


  test("update that selects nothing") {
    def emptyUpdate(user: User, pid: Program.Id): IO[Unit] =
      expect(
        user = user,
        query = s"""
          mutation {
            updateObservations(input: {
              programId: ${pid.asJson}
              SET: {
                constraintSet: {
                  cloudExtinction: ONE_POINT_ZERO
                }
              },
              WHERE: {
                id: { EQ: "o-9999" }
              }
            }) {
              constraintSet {
                cloudExtinction
              }
            }
          }
        """,
        expected =json"""
          {
            "updateObservations": [ ]
          }
        """.asRight
      )

    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- emptyUpdate(pi, pid)
    yield ()
  }

  test("update cloud extinction") {
    oneUpdateTest(
      user   = pi,
      update = """
        constraintSet: {
          cloudExtinction: ONE_POINT_ZERO
        }
      """,
      query = """
        constraintSet {
          cloudExtinction
        }
      """,
      expected = json"""
        {
          "updateObservations": [
            {
              "constraintSet": {
                "cloudExtinction": "ONE_POINT_ZERO"
              }
            }
          ]
        }
      """.asRight
    )
  }

  test("update air mass range") {
    oneUpdateTest(
      user   = pi,
      update = """
        constraintSet: {
          elevationRange: {
            airMass: {
              min: 1.1
            }
          }
        }
      """,
      query = """
        constraintSet {
          elevationRange {
            airMass {
              min
              max
            }
          }
        }
      """,
      expected = json"""
        {
          "updateObservations": [
            {
              "constraintSet": {
                "elevationRange": {
                  "airMass": {
                    "min": 1.10,
                    "max": 2.00
                  }
                }
              }
            }
          ]
        }
      """.asRight
      )
  }

  test("hour angle constraint violation") {

    oneUpdateTest(
      user   = pi,
      update = """
        constraintSet: {
          elevationRange: {
            hourAngle: {
              minHours: -1.0
            }
          }
        }
      """,
      query = """
        constraintSet {
          elevationRange {
            airMass {
              min
              max
            }
          }
        }
      """,
      expected = ObservationService.MissingHourAngleConstraint.message.asLeft
    )

  }

  test("switch elevation range constraint type") {
    oneUpdateTest(
      user   = pi,
      update = """
        constraintSet: {
          elevationRange: {
            hourAngle: {
              minHours: -1.0,
              maxHours:  1.0
            }
          }
        }
      """,
      query = """
        constraintSet {
          elevationRange {
            airMass {
              min
              max
            }
            hourAngle {
              minHours
              maxHours
            }
          }
        }
      """,
      expected = json"""
        {
          "updateObservations": [
            {
              "constraintSet": {
                "elevationRange": {
                  "airMass": null,
                  "hourAngle": {
                    "minHours": -1.00,
                    "maxHours": 1.00
                  }
                }
              }
            }
          ]
        }
      """.asRight
    )

  }

  test("conflicting elevation range updates") {

    oneUpdateTest(
      user   = pi,
      update = """
        constraintSet: {
          elevationRange: {
            airMass: {
              min: 1.1
            },
            hourAngle: {
              minHours: -1.0
            }
          }
        }
      """,
      query = """
        constraintSet {
          elevationRange {
            airMass {
              min
              max
            }
            hourAngle {
              minHours
              maxHours
            }
          }
        }
      """,
      expected = "Argument 'input.SET.constraintSet.elevationRange' is invalid: Only one of airMass or hourAngle may be specified.".asLeft
    )

  }

  test("set explicit base in existing observation without one") {

    oneUpdateTest(
      user = pi,
      update = """
        targetEnvironment: {
          explicitBase: {
            ra: { hms: "1:00:00"},
            dec: { dms: "2:00:00"}
          }
        }
      """,
      query = """
        targetEnvironment {
          explicitBase {
            ra { hours }
            dec { degrees }
          }
        }
      """,
      expected = json"""
        {
          "updateObservations": [
            {
              "targetEnvironment": {
                "explicitBase": {
                  "ra": {
                    "hours": 1.0
                  },
                  "dec": {
                    "degrees": 2.0
                  }
                }
              }
            }
          ]
        }
      """.asRight
    )

  }


  test("set an asterism in existing observation without one") {

    def update(tid: Target.Id) = s"""
      targetEnvironment: {
        asterism: [ "${tid.show}" ]
      }
    """

    val query = """
      targetEnvironment {
        asterism {
          id
        }
      }
    """

    def expected(tid: Target.Id) = json"""
      {
        "updateObservations": [
          {
            "targetEnvironment": {
              "asterism": [
                {
                  "id": ${tid.show}
                }
              ]
            }
          }
        ]
      }
    """.asRight

    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      tid <- createEmptyTargetAs(pi, pid, "Biff")
      _   <- updateObservation(pi, pid, oid, update(tid), query, expected(tid))
    } yield ()

  }

  test("update an asterism in existing observation") {

    def update(tids: Target.Id*) =
      s"""
        targetEnvironment: {
          asterism: [ "${tids.map(_.show).intercalate("\", \"")}" ]
        }
      """

    val query = """
      targetEnvironment {
        asterism {
          id
        }
      }
    """

    def expected(tids: Target.Id*) = json"""
      {
        "updateObservations": [
          {
            "targetEnvironment": {
              "asterism": ${tids.map(tid => json"""{ "id": ${tid.asJson} }""").asJson}
            }
          }
        ]
      }
    """.asRight

    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      t0  <- createEmptyTargetAs(pi, pid, "Larry")
      t1  <- createEmptyTargetAs(pi, pid, "Curly")
      t2  <- createEmptyTargetAs(pi, pid, "Moe")
      _   <- updateObservation(pi, pid, oid, update(t0, t1), query, expected(t0, t1))
      _   <- updateObservation(pi, pid, oid, update(t1, t2), query, expected(t1, t2))
    } yield ()

  }

  test("update explicit ra in observation with existing explicit base") {

    val update1 = """
      targetEnvironment: {
        explicitBase: {
          ra: { hms: "1:00:00" },
          dec: { dms: "2:00:00"}
        }
      }
    """

    val update2 ="""
      targetEnvironment: {
        explicitBase: {
          ra: { hms: "3:00:00"}
        }
      }
    """

    val query = """
      targetEnvironment {
        explicitBase {
          ra { hours }
          dec { degrees }
        }
      }
    """

    val expected1 = json"""
      {
        "updateObservations": [
          {
            "targetEnvironment": {
              "explicitBase": {
                "ra": {
                  "hours": 1.0
                },
                "dec": {
                  "degrees": 2.0
                }
              }
            }
          }
        ]
      }
    """.asRight

    val expected2 = json"""
      {
        "updateObservations": [
          {
            "targetEnvironment": {
              "explicitBase": {
                "ra": {
                  "hours": 3.0
                },
                "dec": {
                  "degrees": 2.0
                }
              }
            }
          }
        ]
      }
    """.asRight

    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- updateObservation(pi, pid, oid, update1, query, expected1)
      _   <- updateObservation(pi, pid, oid, update2, query, expected2)
    } yield ()

  }

  test("delete explicit base") {

    val update1 = """
      targetEnvironment: {
        explicitBase: {
          ra: { hms: "1:00:00" },
          dec: { dms: "2:00:00"}
        }
      }
    """

    val update2 = """
      targetEnvironment: {
        explicitBase: null
      }
    """

    val query = """
      targetEnvironment {
        explicitBase {
          ra { hours }
          dec { degrees }
        }
      }
    """

    val expected1 = json"""
      {
        "updateObservations": [
          {
            "targetEnvironment": {
              "explicitBase": {
                "ra": {
                  "hours": 1.0
                },
                "dec": {
                  "degrees": 2.0
                }
              }
            }
          }
        ]
      }
    """.asRight

    val expected2 = json"""
      {
        "updateObservations": [
          {
            "targetEnvironment": {
              "explicitBase": null
            }
          }
        ]
      }
    """.asRight

    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- updateObservation(pi, pid, oid, update1, query, expected1)
      _   <- updateObservation(pi, pid, oid, update2, query, expected2)
    } yield ()

  }

  test("fail to set (only) explicit ra in existing observation without explicit base") {

    val update = """
      targetEnvironment: {
        explicitBase: {
          ra: { hms: "1:00:00"}
        }
      }
    """

    val query = """
      targetEnvironment {
        explicitBase {
          ra { hours }
          dec { degrees }
        }
      }
    """

    val expected = ObservationService.BothExplicitCoordinatesConstraint.message.asLeft

    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- updateObservation(pi, pid, oid, update, query, expected)
    } yield ()

  }

  test("set visualization time") {

    val update   = """
      visualizationTime: "2022-08-30 17:18:00"
    """

    val query    = "visualizationTime"

    val expected = json"""
      {
        "updateObservations": [
          {
            "visualizationTime": "2022-08-30 17:18:00"
          }
        ]
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)
  }

  test("set visualization time ISO-8601") {

    val update = """
      visualizationTime: "2022-08-30 17:18:00"
    """

    val query = "visualizationTime"

    val expected = json"""
      {
        "updateObservations": [
          {
            "visualizationTime": "2022-08-30 17:18:00"
          }
        ]
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)
  }

  test("delete visualization time") {

    val update0  = """
      visualizationTime: "2022-08-30 17:18:00"
    """

    val update1 = """
      visualizationTime: null
    """

    val query    = "visualizationTime"

    val expected0 = json"""
      {
        "updateObservations": [
          {
            "visualizationTime": "2022-08-30 17:18:00"
          }
        ]
      }
    """.asRight

    val expected1 = json"""
      {
        "updateObservations": [
          {
            "visualizationTime": null
          }
        ]
      }
    """.asRight

    multiUpdateTest(pi, List((update0, query, expected0), (update1, query, expected1)))

  }

}

trait UpdateConstraintSetOps { this: OdbSuite =>

  def updateObservation(
    user:     User,
    pid:      Program.Id,
    oid:      Observation.Id,
    update:   String,
    query:    String,
    expected: Either[String, Json]
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateObservations(input: {
            programId: ${pid.asJson}
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
      """,
      expected = expected.leftMap(msg => List(msg))
    )

}