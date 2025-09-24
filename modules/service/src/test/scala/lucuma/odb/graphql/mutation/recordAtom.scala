// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.util.IdempotencyKey
import org.scalacheck.Arbitrary.arbitrary

class recordAtom extends OdbSuite {

  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(service)

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
      service,
      vid => s"""
        mutation {
          recordAtom(input: {
            visitId: ${vid.asJson},
            instrument: GMOS_NORTH,
            sequenceType: ACQUISITION
          }) {
            atomRecord {
              visit {
                id
              }
              sequenceType
              steps {
                matches {
                  id
                }
              }
            }
          }
        }
      """,
      vid => json"""
        {
          "recordAtom": {
            "atomRecord": {
              "visit": {
                "id": ${vid.asJson}
              },
              "sequenceType": "ACQUISITION",
              "steps": {
                "matches": []
              }
            }
          }
        }
      """.asRight
    )
  }

  test("recordAtom - GmosSouth") {
    recordAtomTest(
      ObservingModeType.GmosSouthLongSlit,
      service,
      vid => s"""
        mutation {
          recordAtom(input: {
            visitId: ${vid.asJson},
            instrument: GMOS_SOUTH,
            sequenceType: ACQUISITION
          }) {
            atomRecord {
              visit {
                id
              }
              sequenceType
              steps {
                matches {
                  id
                }
              }
            }
          }
        }
      """,
      vid => json"""
        {
          "recordAtom": {
            "atomRecord": {
              "visit": {
                "id": ${vid.asJson}
              },
              "sequenceType": "ACQUISITION",
              "steps": {
                "matches": []
              }
            }
          }
        }
      """.asRight
    )
  }

  test("recordStep - mix up") {
    recordAtomTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      vid => s"""
        mutation {
          recordAtom(input: {
            visitId: ${vid.asJson},
            instrument: GMOS_SOUTH,
            sequenceType: ACQUISITION
          }) {
            atomRecord {
              visit {
                id
              }
            }
          }
        }
      """,
      vid => s"Visit '$vid' not found or is not a GMOS South visit".asLeft
    )
  }

  test("recordAtom - generated id") {
    import lucuma.core.util.arb.ArbUid.given
    val gen = arbitrary[Atom.Id].sample.get

    recordAtomTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      vid => s"""
        mutation {
          recordAtom(input: {
            visitId: ${vid.asJson},
            instrument: GMOS_NORTH,
            sequenceType: ACQUISITION,
            generatedId: ${gen.asJson}
          }) {
            atomRecord {
              generatedId
            }
          }
        }
      """,
      _ => json"""
        {
          "recordAtom": {
            "atomRecord": {
              "generatedId": ${gen.asJson}
            }
          }
        }
      """.asRight
    )
  }

  test("recordAtom - idempotencyKey"):
    val idm = IdempotencyKey.FromString.getOption("7304956b-45ab-45b6-8db1-ae6f743b519c").get

    def recordAtom(vid: Visit.Id): IO[Atom.Id] =
      query(
        user  = service,
        query = s"""
          mutation {
            recordAtom(input: {
              visitId: "$vid"
              instrument: GMOS_NORTH
              sequenceType: SCIENCE
              idempotencyKey: "${IdempotencyKey.FromString.reverseGet(idm)}"
            }) {
              atomRecord { id }
            }
          }
        """
      ).map: js =>
        js.hcursor
          .downFields("recordAtom", "atomRecord", "id")
          .require[Atom.Id]

    assertIOBoolean:
      for
        (_, _, v) <- recordVisit(ObservingModeType.GmosNorthLongSlit, service)
        a0        <- recordAtom(v)
        a1        <- recordAtom(v)
      yield a0 === a1

}
