// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.data.EitherNec
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.catalog.goa.GoaClient
import lucuma.catalog.goa.GoaClientMock
import lucuma.catalog.goa.GoaParams
import lucuma.catalog.goa.GoaQueryError
import lucuma.catalog.goa.GoaSummaryRecord
import lucuma.core.enums.GeminiCallForProposalsType.DemoScience
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.core.model.User
import lucuma.odb.data.OdbError
import lucuma.odb.util.Codecs.program_id
import skunk.syntax.all.*

class refreshGoaDuplication extends OdbSuite:

  val pi: User    = TestUsers.Standard.pi(nextId, nextId)
  val pi2: User   = TestUsers.Standard.pi(nextId, nextId)
  val staff: User = TestUsers.Standard.staff(nextId, nextId)

  lazy val validUsers: List[User] = List(pi, pi2, staff)

  /** What the archive is doing right now, as each test needs it. */
  private enum Archive:
    case Down
    case Holding(json: String)

  private val archive: Ref[IO, Archive] =
    Ref.unsafe(Archive.Holding("[]"))

  override protected def goaClient: IO[GoaClient[IO]] =
    IO.pure:
      new GoaClient[IO]:
        def query(params: GoaParams): IO[EitherNec[GoaQueryError, List[GoaSummaryRecord]]] =
          archive.get.flatMap:
            case Archive.Down          =>
              IO.pure(GoaQueryError.NetworkError(new RuntimeException("connection refused")).leftNec)
            case Archive.Holding(json) =>
              GoaClientMock.fromJson[IO](json).query(params)

  private def records(names: String*): String =
    names
      .map(n => s"""{ "name": "$n", "instrument": "GMOS-N", "observation_type": "OBJECT" }""")
      .mkString("[", ",", "]")

  private def observation(user: User = pi): IO[Observation.Id] =
    for
      pid <- createProgramAs(user)
      tid <- createTargetAs(user, pid)
      oid <- createGmosNorthImagingObservationAs(user, pid, tid)
    yield oid

  /** A program carrying a proposal that is ready to be submitted. */
  private def proposedProgram: IO[Program.Id] =
    for
      pid <- createProgramWithUsPi(pi)
      cid <- createGeminiCallForProposalsAs(staff, DemoScience, Semester.unsafeFromString("2025A"))
      _   <- addDemoScienceProposal(pi, pid, cid)
    yield pid

  /**
   * Submits a proposal and then adds an observation to it.  Submitting the
   * other way around would require the observation to be fully defined, which
   * is beside the point here.
   */
  private def observationUnderSubmittedProposal: IO[Observation.Id] =
    for
      pid <- proposedProgram
      _   <- submitProposal(pi, pid)
      tid <- createTargetAs(pi, pid)
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
    yield oid

  /**
   * Submission is what freezes the snapshot, and the API will not submit a
   * program whose observations are undefined -- which is a different rule than
   * the one under test.  So the status is set directly, to reach the state a
   * PI reaches by submitting a finished proposal.
   */
  private def markSubmitted(pid: Program.Id): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"update t_program set c_proposal_status = 'submitted' where c_program_id = $program_id".command
      )(pid).void

  private def storedDuplication(oid: Observation.Id, fields: String): IO[Json] =
    query(
      user  = pi,
      query = s"""
        query {
          observation(observationId: ${oid.asJson}) {
            goaDuplication { $fields }
          }
        }
      """
    ).map(_.hcursor.downFields("observation", "goaDuplication").focus.get)

  test("refresh stores the search result and returns it"):
    for
      oid <- observation()
      _   <- archive.set(Archive.Holding(records("a.fits", "b.fits")))
      js  <- refreshGoaDuplicationAs(pi, oid, "state matchCount saturated error matches { name }")
    yield assertEquals(
      js,
      json"""
        {
          "state": "CHECKED",
          "matchCount": 2,
          "saturated": false,
          "error": null,
          "matches": [{ "name": "a.fits" }, { "name": "b.fits" }]
        }
      """
    )

  test("re-checking replaces the previous result rather than adding to it"):
    for
      oid <- observation()
      _   <- archive.set(Archive.Holding(records("a.fits", "b.fits")))
      _   <- refreshGoaDuplicationAs(pi, oid)
      _   <- archive.set(Archive.Holding(records("c.fits")))
      js  <- refreshGoaDuplicationAs(pi, oid, "matchCount matches { name }")
      db  <- storedDuplication(oid, "matchCount matches { name }")
    yield
      assertEquals(js, json"""{ "matchCount": 1, "matches": [{ "name": "c.fits" }] }""")
      assertEquals(db, js)

  test("refresh is rejected once the proposal has been submitted"):
    for
      oid <- observationUnderSubmittedProposal
      _   <- archive.set(Archive.Holding(records("a.fits")))
      _   <- interceptOdbError(refreshGoaDuplicationAs(pi, oid)):
               case OdbError.InvalidObservation(_, Some(msg)) =>
                 assert(msg.contains("submitted"), msg)
      db  <- storedDuplication(oid, "state matchCount matches { name }")
    yield assertEquals(
      db,
      json"""{ "state": "NOT_CHECKED", "matchCount": 0, "matches": [] }"""
    )

  test("submitting freezes the snapshot the PI last saw"):
    for
      pid    <- proposedProgram
      tid    <- createTargetAs(pi, pid)
      oid    <- createGmosNorthImagingObservationAs(pi, pid, tid)
      _      <- archive.set(Archive.Holding(records("a.fits")))
      _      <- refreshGoaDuplicationAs(pi, oid)
      before <- storedDuplication(oid, "state matchCount matches { name }")
      _      <- markSubmitted(pid)
      _      <- archive.set(Archive.Holding(records("x.fits", "y.fits")))
      _      <- interceptOdbError(refreshGoaDuplicationAs(pi, oid)):
                  case OdbError.InvalidObservation(_, Some(msg)) =>
                    assert(msg.contains("submitted"), msg)
      after  <- storedDuplication(oid, "state matchCount matches { name }")
    yield
      assertEquals(before, json"""{ "state": "CHECKED", "matchCount": 1, "matches": [{ "name": "a.fits" }] }""")
      assertEquals(after, before)

  test("an unreachable archive is reported as an error state, not a failed mutation"):
    for
      oid <- observation()
      _   <- archive.set(Archive.Down)
      js  <- refreshGoaDuplicationAs(pi, oid, "state matchCount error")
    yield
      assertEquals(js.hcursor.downField("state").as[String], Right("ERROR"))
      assert(js.hcursor.downField("error").as[String].exists(_.contains("Network error")))

  test("an unreachable archive leaves the last good result readable"):
    for
      oid <- observation()
      _   <- archive.set(Archive.Holding(records("a.fits", "b.fits")))
      _   <- refreshGoaDuplicationAs(pi, oid)
      _   <- archive.set(Archive.Down)
      _   <- refreshGoaDuplicationAs(pi, oid)
      db  <- storedDuplication(oid, "state matchCount matches { name }")
    yield assertEquals(
      db,
      json"""
        {
          "state": "ERROR",
          "matchCount": 2,
          "matches": [{ "name": "a.fits" }, { "name": "b.fits" }]
        }
      """
    )

  test("creating an observation while the archive is unreachable still succeeds"):
    for
      _   <- archive.set(Archive.Down)
      oid <- observation()
      db  <- storedDuplication(oid, "state matchCount error matches { name }")
    yield assertEquals(
      db,
      json"""
        {
          "state": "NOT_CHECKED",
          "matchCount": 0,
          "error": null,
          "matches": []
        }
      """
    )

  test("refresh is rejected for a user who cannot edit the observation"):
    for
      oid <- observation()
      _   <- archive.set(Archive.Holding(records("a.fits")))
      _   <- interceptOdbError(refreshGoaDuplicationAs(pi2, oid)):
               case OdbError.NotAuthorized(_, _) => ()
      db  <- storedDuplication(oid, "state")
    yield assertEquals(db, json"""{ "state": "NOT_CHECKED" }""")
