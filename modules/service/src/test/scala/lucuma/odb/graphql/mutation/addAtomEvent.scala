// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.odb.data.ObservingModeType


class addAtomEvent extends OdbSuite {

  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(service)

  private def recordAtom(
    mode: ObservingModeType,
    user: User
  ):IO[(Observation.Id, Atom.Id)] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
      } yield (oid, aid)

  private def addAtomEventTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Atom.Id => String,
    expected: (Observation.Id, Atom.Id) => Either[String, Json]
  ): IO[Unit] =
    for {
      ids <- recordAtom(mode, user)
      (oid, aid) = ids
      _   <- expect(user, query(aid), expected(oid, aid).leftMap(s => List(s)))
    } yield ()


  test("addAtomEvent") {
    def query(aid: Atom.Id): String =
      s"""
        mutation {
          addAtomEvent(input: {
            atomId:    "$aid",
            atomStage: START_ATOM
          }) {
            event {
              atom {
                id
              }
              atomStage
              observation {
                id
              }
            }
          }
        }
      """

    addAtomEventTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      aid => query(aid),
      (oid, aid) => json"""
      {
        "addAtomEvent": {
          "event": {
            "atom": {
               "id": $aid
            },
            "atomStage": "START_ATOM",
            "observation": {
              "id": $oid
            }
          }
        }
      }
      """.asRight
    )

  }

   test("addAtomEvent - unknown atom") {
    def query: String =
      s"""
        mutation {
          addAtomEvent(input: {
            atomId:    "a-cfebc981-db7e-4c35-964d-6b19aa5ed2d7",
            atomStage: START_ATOM
          }) {
            event {
              atom {
                id
              }
            }
          }
        }
      """

    addAtomEventTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      _ => query,
      (_, _) => s"Atom 'a-cfebc981-db7e-4c35-964d-6b19aa5ed2d7' not found".asLeft
    )

  }

}