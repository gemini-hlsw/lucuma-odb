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
import lucuma.core.model.User
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
      expected = ObservationService.MissingHourAngleConstraintMessage.asLeft
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
        query =
          s"""
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

}