// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.odb.data.ObservingModeType

class recordAtom extends OdbSuite {

  val staff: User = TestUsers.Standard.staff(nextId, nextId)

  override lazy val validUsers: List[User] = List(staff)

  private def recordVisit(
    mode: ObservingModeType,
    user: User
  ): IO[(Program.Id, Observation.Id, Visit.Id)] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
    } yield (pid, oid, vid)

  private def recordAtomTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Visit.Id => String,
    expected: Visit.Id => Either[String, Json]
  ): IO[Unit] =
    for {
      ids <- recordVisit(mode, user)
      (_, _, vid) = ids
      _   <- expect(user, query(vid), expected(vid).leftMap(msg => List(msg)))
    } yield ()

  test("recordAtom - GmosNorth") {
    recordAtomTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosNorthAtom(input: {
            visitId: ${vid.asJson},
            sequenceType: ACQUISITION,
            stepCount: 3
          }) {
            atomRecord {
              visitId
              sequenceType
              stepCount
              steps {
                id
              }
            }
          }
        }
      """,
      vid => json"""
        {
          "recordGmosNorthAtom": {
            "atomRecord": {
              "visitId": ${vid.asJson},
              "sequenceType": "ACQUISITION",
              "stepCount": 3,
              "steps": []
            }
          }
        }
      """.asRight
    )
  }

  test("recordAtom - GmosSouth") {
    recordAtomTest(
      ObservingModeType.GmosSouthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosSouthAtom(input: {
            visitId: ${vid.asJson},
            sequenceType: ACQUISITION,
            stepCount: 3
          }) {
            atomRecord {
              visitId
              sequenceType
              stepCount
              steps {
                id
              }
            }
          }
        }
      """,
      vid => json"""
        {
          "recordGmosSouthAtom": {
            "atomRecord": {
              "visitId": ${vid.asJson},
              "sequenceType": "ACQUISITION",
              "stepCount": 3,
              "steps": []
            }
          }
        }
      """.asRight
    )
  }

  test("recordStep - mix up") {
    recordAtomTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosSouthAtom(input: {
            visitId: ${vid.asJson},
            sequenceType: ACQUISITION,
            stepCount: 3
          }) {
            atomRecord {
              visitId
            }
          }
        }
      """,
      vid => s"Visit '$vid' not found or is not a GMOS South visit".asLeft
    )
  }

}
