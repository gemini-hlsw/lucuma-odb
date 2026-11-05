// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import io.circe.literal.*
import lucuma.core.enums.GeminiCallForProposalsType.RegularSemester
import lucuma.core.enums.Partner
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TacPartnerDecision
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.input.TacFeedbackInput
import lucuma.odb.graphql.input.TacRecommendedTimeInput

class deleteTacFeedback extends OdbSuite with query.ObservingModeSetupOperations:

  val guest   = TestUsers.guest(nextId)
  val pi      = TestUsers.Standard.pi(nextId, nextId)
  val ngoCa   = TestUsers.Standard.ngo(nextId, nextId, Partner.CA)
  val ngoUs   = TestUsers.Standard.ngo(nextId, nextId, Partner.US)
  val staff   = TestUsers.Standard.staff(nextId, nextId)
  val admin   = TestUsers.Standard.admin(nextId, nextId)
  val service = TestUsers.service(nextId)

  lazy val validUsers = List(guest, pi, ngoCa, ngoUs, staff, admin, service)

  def feedback(partner: Partner): TacFeedbackInput =
    TacFeedbackInput(
      partner,
      TacPartnerDecision.Accepted.some,
      PosBigDecimal.unsafeFrom(BigDecimal(1)).some,
      List(TacRecommendedTimeInput(ScienceBand.Band1, TimeSpan.fromHours(BigDecimal(2)).get)).some,
      Nullable.Absent
    )

  // A program whose proposal requests time from the US (70%) and CA (30%),
  // with TAC feedback from both partners.
  def createProgramWithFeedback: IO[Program.Id] =
    for
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 70), (Partner.CA, 30)))
      _   <- setTacFeedbackAs(staff, pid, feedback(Partner.US))
      _   <- setTacFeedbackAs(staff, pid, feedback(Partner.CA))
    yield pid

  def partners(pid: Program.Id): IO[List[String]] =
    query(staff, s"""query { program(programId: "$pid") { tacFeedback { partner } } }""").flatMap: js =>
      js.hcursor
        .downFields("program", "tacFeedback")
        .values
        .toList
        .flatten
        .traverse(_.hcursor.downField("partner").as[String])
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  def notAuthorized(user: lucuma.core.model.User)(fa: IO[Any]): IO[Unit] =
    interceptGraphQL(s"User ${user.id} is not authorized to perform this operation.")(fa)

  test("guest and pi can't delete TAC feedback"):
    createProgramWithFeedback.flatMap: pid =>
      List(guest, pi).traverse_(user => notAuthorized(user)(deleteTacFeedbackAs(user, pid, Partner.US))) *>
      partners(pid).map(assertEquals(_, List("CA", "US")))

  test("ngo can delete TAC feedback for its own partner"):
    createProgramWithFeedback.flatMap: pid =>
      deleteTacFeedbackAs(ngoCa, pid, Partner.CA).map(assert(_)) *>
      partners(pid).map(assertEquals(_, List("US")))

  test("ngo can't delete TAC feedback for another partner"):
    createProgramWithFeedback.flatMap: pid =>
      notAuthorized(ngoCa)(deleteTacFeedbackAs(ngoCa, pid, Partner.US)) *>
      partners(pid).map(assertEquals(_, List("CA", "US")))

  test("ngo can't delete TAC feedback for a program it cannot read"):
    createProgramWithFeedback.flatMap: pid =>
      // Dropping CA's split hides the program from the CA NGO.
      addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100), (Partner.CA, 0))) *>
      notAuthorized(ngoCa)(deleteTacFeedbackAs(ngoCa, pid, Partner.CA)) *>
      partners(pid).map(assertEquals(_, List("CA", "US")))

  test("staff can delete TAC feedback for a partner that no longer has a time split"):
    createProgramWithFeedback.flatMap: pid =>
      addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100), (Partner.CA, 0))) *>
      deleteTacFeedbackAs(staff, pid, Partner.CA).map(assert(_)) *>
      partners(pid).map(assertEquals(_, List("US")))

  test("staff, admin and service can delete TAC feedback for any partner"):
    List((staff, Partner.US), (admin, Partner.CA), (service, Partner.US)).traverse_ : (user, partner) =>
      createProgramWithFeedback.flatMap: pid =>
        deleteTacFeedbackAs(user, pid, partner).map(assert(_))

  test("deleting TAC feedback also deletes its recommended times"):
    createProgramWithFeedback.flatMap: pid =>
      deleteTacFeedbackAs(staff, pid, Partner.US).map(assert(_)) *>
      setTacFeedbackAs(staff, pid, feedback(Partner.US).copy(recommendedTimes = none)) *>
      expect(
        user     = staff,
        query    = s"""query { program(programId: "$pid") { tacFeedback { partner recommendedTimes { band } } } }""",
        expected = json"""
          {
            "program": {
              "tacFeedback": [
                { "partner": "CA", "recommendedTimes": [ { "band": "BAND1" } ] },
                { "partner": "US", "recommendedTimes": [] }
              ]
            }
          }
        """.asRight
      )

  test("deleting missing TAC feedback returns false"):
    createProgramWithFeedback.flatMap: pid =>
      deleteTacFeedbackAs(staff, pid, Partner.US).map(assert(_)) *>
      deleteTacFeedbackAs(staff, pid, Partner.US).map(b => assert(!b))

  test("delete TAC feedback with a proposal reference"):
    for
      cid <- createGeminiCallForProposalsAs(staff, RegularSemester, Semester.unsafeFromString("2025A"))
      pid <- createProgramWithUsPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addSubmissionPrerequisitesAs(pi, pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100)))
      _   <- setTacFeedbackAs(staff, pid, feedback(Partner.US))
      ref <- submitProposal(pi, pid)
      _   <- expect(
               user     = ngoUs,
               query    = s"""
                 mutation {
                   deleteTacFeedback(input: {
                     proposalReference: "${ref.label}"
                     partner: US
                   }) {
                     result
                   }
                 }
               """,
               expected = json"""{ "deleteTacFeedback": { "result": true } }""".asRight
             )
      _   <- partners(pid).map(assertEquals(_, Nil))
    yield ()
