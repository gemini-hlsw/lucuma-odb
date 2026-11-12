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
import lucuma.core.enums.ProposalStatus
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TacPartnerDecision
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.input.AllocationInput
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

  val US = TimeAccountingCategory.US
  val CA = TimeAccountingCategory.CA

  def feedback(category: TimeAccountingCategory): TacFeedbackInput =
    TacFeedbackInput(
      category,
      TacPartnerDecision.Accepted.some,
      PosBigDecimal.unsafeFrom(BigDecimal(1)).some,
      List(TacRecommendedTimeInput(ScienceBand.Band1, TimeSpan.fromHours(BigDecimal(2)).get)).some,
      Nullable.Absent
    )

  // A program whose submitted proposal requests time from the US (70%) and
  // CA (30%), with TAC feedback from both partners.
  def createProgramWithFeedback: IO[Program.Id] =
    for
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 70), (Partner.CA, 30)))
      _   <- setProposalSubmittedDirectly(pid)
      _   <- setTacFeedbackAs(staff, pid, feedback(US))
      _   <- setTacFeedbackAs(staff, pid, feedback(CA))
    yield pid

  def categories(pid: Program.Id): IO[List[String]] =
    query(staff, s"""query { program(programId: "$pid") { tacFeedback { timeAccountingCategory } } }""").flatMap: js =>
      js.hcursor
        .downFields("program", "tacFeedback")
        .values
        .toList
        .flatten
        .traverse(_.hcursor.downField("timeAccountingCategory").as[String])
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  // The program's allocations, as (category, band) pairs.
  def allocations(pid: Program.Id): IO[List[(String, String)]] =
    query(staff, s"""query { program(programId: "$pid") { allocations { category scienceBand } } }""").flatMap: js =>
      js.hcursor
        .downFields("program", "allocations")
        .values
        .toList
        .flatten
        .traverse(a => (a.hcursor.downField("category").as[String], a.hcursor.downField("scienceBand").as[String]).tupled)
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  // Drops CA's split to 0%.  A submitted proposal cannot be edited, so it is
  // briefly returned to NOT_SUBMITTED.
  def dropCaSplit(pid: Program.Id): IO[Unit] =
    setProposalStatusDirectly(pid, ProposalStatus.NotSubmitted) *>
    addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100), (Partner.CA, 0))) *>
    setProposalSubmittedDirectly(pid)

  def notAuthorized(user: lucuma.core.model.User)(fa: IO[Any]): IO[Unit] =
    interceptGraphQL(s"User ${user.id} is not authorized to perform this operation.")(fa)

  test("guest and pi can't delete TAC feedback"):
    createProgramWithFeedback.flatMap: pid =>
      List(guest, pi).traverse_(user => notAuthorized(user)(deleteTacFeedbackAs(user, pid, US))) *>
      categories(pid).map(assertEquals(_, List("CA", "US")))

  test("ngo can delete TAC feedback for its own partner"):
    createProgramWithFeedback.flatMap: pid =>
      deleteTacFeedbackAs(ngoCa, pid, CA).map(assert(_)) *>
      categories(pid).map(assertEquals(_, List("US")))

  test("ngo can't delete TAC feedback for another partner"):
    createProgramWithFeedback.flatMap: pid =>
      notAuthorized(ngoCa)(deleteTacFeedbackAs(ngoCa, pid, US)) *>
      categories(pid).map(assertEquals(_, List("CA", "US")))

  test("ngo can't delete TAC feedback for a program it cannot read"):
    createProgramWithFeedback.flatMap: pid =>
      // Dropping CA's split hides the program from the CA NGO, once CA's
      // recommended time (an allocation, which also makes the program
      // visible) is removed as well.
      setTacFeedbackAs(staff, pid, TacFeedbackInput(CA, none, none, Nil.some, Nullable.Absent)) *>
      dropCaSplit(pid) *>
      notAuthorized(ngoCa)(deleteTacFeedbackAs(ngoCa, pid, CA)) *>
      categories(pid).map(assertEquals(_, List("CA", "US")))

  test("staff can delete TAC feedback for a partner that no longer has a time split"):
    createProgramWithFeedback.flatMap: pid =>
      dropCaSplit(pid) *>
      deleteTacFeedbackAs(staff, pid, CA).map(assert(_)) *>
      categories(pid).map(assertEquals(_, List("US")))

  test("staff, admin and service can delete TAC feedback for any partner"):
    List((staff, US), (admin, CA), (service, US)).traverse_ : (user, category) =>
      createProgramWithFeedback.flatMap: pid =>
        deleteTacFeedbackAs(user, pid, category).map(assert(_))

  test("deleting TAC feedback also deletes its recommended times"):
    createProgramWithFeedback.flatMap: pid =>
      deleteTacFeedbackAs(staff, pid, US).map(assert(_)) *>
      setTacFeedbackAs(staff, pid, feedback(US).copy(recommendedTimes = none)) *>
      expect(
        user     = staff,
        query    = s"""query { program(programId: "$pid") { tacFeedback { timeAccountingCategory recommendedTimes { band } } } }""",
        expected = json"""
          {
            "program": {
              "tacFeedback": [
                { "timeAccountingCategory": "CA", "recommendedTimes": [ { "band": "BAND1" } ] },
                { "timeAccountingCategory": "US", "recommendedTimes": [] }
              ]
            }
          }
        """.asRight
      )

  test("deleting TAC feedback deletes the allocations carrying its recommended times"):
    createProgramWithFeedback.flatMap: pid =>
      // A US allocation without a recommended time is unaffected.
      setAllocationsAs(staff, pid, List(
        AllocationInput(US, ScienceBand.Band1, TimeSpan.fromHours(2).get),
        AllocationInput(US, ScienceBand.Band2, TimeSpan.fromHours(3).get),
        AllocationInput(CA, ScienceBand.Band1, TimeSpan.fromHours(2).get)
      )) *>
      deleteTacFeedbackAs(staff, pid, US).map(assert(_)) *>
      allocations(pid).map(assertEquals(_, List(("CA", "BAND1"), ("US", "BAND2"))))

  test("deleting the proposal deletes its TAC feedback and recommended times"):
    createProgramWithFeedback.flatMap: pid =>
      setOneAllocationAs(staff, pid, US, ScienceBand.Band2, TimeSpan.fromHours(3).get) *>
      deleteProposal(staff, pid).map(assert(_)) *>
      categories(pid).map(cs => assertEquals(cs, Nil)) *>
      allocations(pid).map(assertEquals(_, List(("US", "BAND2"))))

  test("TAC feedback can only be deleted while the proposal is submitted"):
    createProgramWithFeedback.flatMap: pid =>
      for
        _ <- setProposalStatusDirectly(pid, ProposalStatus.Accepted)
        _ <- interceptOdbError(deleteTacFeedbackAs(staff, pid, US)):
               case OdbError.InvalidArgument(Some(m)) =>
                 assertEquals(m, s"TAC feedback for program $pid cannot be changed while its proposal status is ACCEPTED.")
        _ <- categories(pid).map(assertEquals(_, List("CA", "US")))
      yield ()

  test("staff can delete TAC feedback for a category that is not a partner"):
    createProgramWithFeedback.flatMap: pid =>
      setTacFeedbackAs(staff, pid, feedback(TimeAccountingCategory.GT)) *>
      notAuthorized(ngoUs)(deleteTacFeedbackAs(ngoUs, pid, TimeAccountingCategory.GT)) *>
      deleteTacFeedbackAs(staff, pid, TimeAccountingCategory.GT).map(assert(_)) *>
      categories(pid).map(assertEquals(_, List("CA", "US")))

  test("deleting missing TAC feedback returns false"):
    createProgramWithFeedback.flatMap: pid =>
      deleteTacFeedbackAs(staff, pid, US).map(assert(_)) *>
      deleteTacFeedbackAs(staff, pid, US).map(b => assert(!b))

  test("delete TAC feedback with a proposal reference"):
    for
      cid <- createGeminiCallForProposalsAs(staff, RegularSemester, Semester.unsafeFromString("2025A"))
      pid <- createProgramWithUsPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addSubmissionPrerequisitesAs(pi, pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100)))
      ref <- submitProposal(pi, pid)
      _   <- setTacFeedbackAs(staff, pid, feedback(US))
      _   <- expect(
               user     = ngoUs,
               query    = s"""
                 mutation {
                   deleteTacFeedback(input: {
                     proposalReference: "${ref.label}"
                     timeAccountingCategory: US
                   }) {
                     result
                   }
                 }
               """,
               expected = json"""{ "deleteTacFeedback": { "result": true } }""".asRight
             )
      _   <- categories(pid).map(assertEquals(_, Nil))
    yield ()
