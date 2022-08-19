// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.input.CoordinatesInput
import lucuma.odb.service.ObservationService

class updateObservations extends OdbSuite
                            with CreateProgramOps
                            with CreateObservationOps
                            with UpdateConstraintSetOps {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  def constraintSetUpdateTest(
    update:   String,
    query:    String,
    expected: Either[String, Json]
  ): IO[Unit] =
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- updateConstraintSet(pi, oid, update, query, expected)
    } yield ()


  test("update cloud extinction") {
    constraintSetUpdateTest(
      update   = "cloudExtinction: ONE_POINT_ZERO",
      query    = "cloudExtinction",
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
    constraintSetUpdateTest(
      update = """
        elevationRange: {
          airMass: {
            min: 1.1
          }
        }
      """,
      query = """
        elevationRange {
          airMass {
            min
            max
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

    constraintSetUpdateTest(
      update = """
        elevationRange: {
          hourAngle: {
            minHours: -1.0
          }
        }
      """,
      query = """
        elevationRange {
          airMass {
            min
            max
          }
        }
      """,
      expected = ObservationService.MissingHourAngleConstraint.message.asLeft
    )

  }

  test("switch elevation range constraint type") {
    constraintSetUpdateTest(
      update = """
        elevationRange: {
          hourAngle: {
            minHours: -1.0,
            maxHours:  1.0
          }
        }
      """,
      query = """
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

    constraintSetUpdateTest(
      update =
        """
        elevationRange: {
          airMass: {
            min: 1.1
          },
          hourAngle: {
            minHours: -1.0
          }
        }
      """,
      query =
        """
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
      """,
      expected = "Argument 'input.SET.constraintSet.elevationRange' is invalid: Only one of airMass or hourAngle may be specified.".asLeft
    )

  }

  test("set explicit base in existing observation without one") {

    val update = """
      explicitBase: {
        ra: { hms: "1:00:00"},
        dec: { dms: "2:00:00"}
      }
    """

    val query = """
      explicitBase {
        ra { hours }
        dec { degrees }
      }
    """

    val expected = json"""
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

    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- updateExplicitBase(pi, oid, update, query, expected)
    } yield ()

  }

  test("update explicit ra in observation with existing explicit base") {

    val update1 = """
      explicitBase: {
        ra: { hms: "1:00:00" },
        dec: { dms: "2:00:00"}
      }
    """

    val update2 ="""
      explicitBase: {
        ra: { hms: "3:00:00"}
      }
    """

    val query = """
      explicitBase {
        ra { hours }
        dec { degrees }
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
      _   <- updateExplicitBase(pi, oid, update1, query, expected1)
      _   <- updateExplicitBase(pi, oid, update2, query, expected2)
    } yield ()

  }

  test("delete explicit base") {

    val update1 = """
      explicitBase: {
        ra: { hms: "1:00:00" },
        dec: { dms: "2:00:00"}
      }
    """

    val update2 = """
      explicitBase: null
    """

    val query = """
      explicitBase {
        ra { hours }
        dec { degrees }
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
      _ <- updateExplicitBase(pi, oid, update1, query, expected1)
      _ <- updateExplicitBase(pi, oid, update2, query, expected2)
    } yield ()

  }

  test("fail to set (only) explicit ra in existing observation without explicit base") {

    val update = """
      explicitBase: {
        ra: { hms: "1:00:00"}
      }
    """

    val query = """
      explicitBase {
        ra { hours }
        dec { degrees }
      }
    """

    val expected = ObservationService.BothExplicitCoordinatesConstraint.message.asLeft

    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _ <- updateExplicitBase(pi, oid, update, query, expected)
    } yield ()

  }

}

trait UpdateConstraintSetOps { this: OdbSuite =>

  def updateConstraintSet(
    user:     User,
    oid:      Observation.Id,
    update:   String,
    query:    String,
    expected: Either[String, Json]
  ): IO[Unit] = {
      expect(
        user = user,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                constraintSet: {
                  $update
                }
              },
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              constraintSet {
                $query
              }
            }
          }
        """,
        expected = expected.leftMap(msg => List(msg))
      )

  }

  def updateExplicitBase(
    user:     User,
    oid:      Observation.Id,
    update:   String,
    query:    String,
    expected: Either[String, Json]
  ): IO[Unit] = {
    expect(
      user = user,
      query = s"""
        mutation {
          updateObservations(input: {
            SET: {
              targetEnvironment: {
                $update
              }
            },
            WHERE: {
              id: { EQ: ${oid.asJson} }
            }
          }) {
            targetEnvironment {
              $query
            }
          }
        }
      """,
      expected = expected.leftMap(msg => List(msg))
    )

  }
}