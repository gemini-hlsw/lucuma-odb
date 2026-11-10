// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.data.Ior
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.GeminiCallForProposalsType.RegularSemester
import lucuma.core.enums.Partner
import lucuma.core.enums.ProposalStatus
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TacPartnerDecision
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Observation
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.input.AllocationInput
import lucuma.odb.graphql.input.TacFeedbackInput
import lucuma.odb.graphql.input.TacRecommendedTimeInput

class setTacFeedback extends OdbSuite with query.ObservingModeSetupOperations:

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

  def feedback(
    category: TimeAccountingCategory,
    decision: TacPartnerDecision = TacPartnerDecision.Accepted,
    ranking:  BigDecimal         = BigDecimal("1.5"),
    times:    List[(ScienceBand, BigDecimal)] = List(ScienceBand.Band1 -> BigDecimal(2)),
    comments: String             = "Great science."
  ): TacFeedbackInput =
    TacFeedbackInput(
      category,
      decision.some,
      PosBigDecimal.unsafeFrom(ranking).some,
      recommendedTimes(times*).some,
      Nullable.NonNull(NonEmptyString.unsafeFrom(comments))
    )

  def recommendedTimes(times: (ScienceBand, BigDecimal)*): List[TacRecommendedTimeInput] =
    times.toList.map((band, hours) => TacRecommendedTimeInput(band, TimeSpan.fromHours(hours).get))

  // Expected recommended times, as (band, hours) pairs.
  def band1(hours: String): List[(String, BigDecimal)] =
    List("BAND1" -> BigDecimal(hours))

  def partial(category: TimeAccountingCategory): TacFeedbackInput =
    TacFeedbackInput(category, none, none, none, Nullable.Absent)

  def feedbackJson(
    category: String,
    decision: String,
    ranking:  BigDecimal,
    times:    List[(String, BigDecimal)],
    comments: String
  ): Json =
    feedbackJson(category, decision, ranking, times, comments.some)

  def feedbackJson(
    category: String,
    decision: String,
    ranking:  BigDecimal,
    times:    List[(String, BigDecimal)],
    comments: Option[String]
  ): Json =
    val timesJson = times.map((band, hours) => json"""{ "band": $band, "time": { "hours": $hours } }""")
    json"""
      {
        "timeAccountingCategory": $category,
        "decision": $decision,
        "ranking": $ranking,
        "recommendedTimes": $timesJson,
        "comments": $comments
      }
    """

  def setResult(f: Json): Json =
    json"""{ "setTacFeedback": { "tacFeedback": $f } }"""

  def submitted(pid: Program.Id): IO[Program.Id] =
    setProposalSubmittedDirectly(pid).as(pid)

  // A program with a submitted proposal requesting time from the US (70%) and
  // CA (30%).
  def createProgramWithProposal(splits: List[(Partner, Int)] = List((Partner.US, 70), (Partner.CA, 30))): IO[Program.Id] =
    for
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = splits)
      _   <- submitted(pid)
    yield pid

  // A program with a submitted proposal to a call of the given type, with the
  // given PI affiliation and proposal type properties.
  def createProgramWithCall(
    callType:  GeminiCallForProposalsType,
    callProps: String,
    piLink:    PartnerLink = PartnerLink.HasGeminiPartner(Partner.US)
  ): IO[Program.Id] =
    for
      cid <- createGeminiCallForProposalsAs(staff, callType)
      pid <- createProgramWithPiAffiliation(pi, piLink)
      _   <- addProposal(pi, pid, cid.some, callProps.some)
      _   <- submitted(pid)
    yield pid

  // A Regular Semester queue proposal from a US PI requesting only US time
  // (other partners would need matching collaborators), ready to submit.
  def createSubmittableProposal: IO[Program.Id] =
    for
      cid <- createGeminiCallForProposalsAs(staff, RegularSemester, Semester.unsafeFromString("2025A"))
      pid <- createProgramWithUsPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addSubmissionPrerequisitesAs(pi, pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100)))
    yield pid

  def notAuthorized(user: lucuma.core.model.User)(fa: IO[Any]): IO[Unit] =
    interceptGraphQL(s"User ${user.id} is not authorized to perform this operation.")(fa)

  def invalid(fa: IO[Any])(message: String): IO[Unit] =
    interceptOdbError(fa):
      case OdbError.InvalidArgument(Some(m)) => assertEquals(m, message)

  def selectTacFeedback(pid: Program.Id): String =
    s"""
      query {
        program(programId: "$pid") {
          tacFeedback {
            timeAccountingCategory
            decision
            ranking
            recommendedTimes { band time { hours } }
            comments
          }
        }
      }
    """

  // The program's allocations, as (category, band, hours) triples.
  def allocations(pid: Program.Id): IO[List[(String, String, BigDecimal)]] =
    query(
      user  = staff,
      query = s"""
        query {
          program(programId: "$pid") {
            allocations { category scienceBand duration { hours } }
          }
        }
      """
    ).flatMap: js =>
      js.hcursor
        .downFields("program", "allocations")
        .values
        .toList
        .flatten
        .traverse: a =>
          val c = a.hcursor
          (c.downField("category").as[String], c.downField("scienceBand").as[String], c.downFields("duration", "hours").as[BigDecimal]).tupled
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  test("guest and pi can't set TAC feedback"):
    createProgramWithProposal().flatMap: pid =>
      List(guest, pi).traverse_ : user =>
        notAuthorized(user)(setTacFeedbackAs(user, pid, feedback(US)))

  test("ngo can set TAC feedback for its own partner"):
    createProgramWithProposal().flatMap: pid =>
      setTacFeedbackAs(ngoCa, pid, feedback(CA)).map: js =>
        assertEquals(js, setResult(feedbackJson("CA", "ACCEPTED", BigDecimal("1.5"), band1("2.000000"), "Great science.")))

  test("ngo can't set TAC feedback for another partner"):
    createProgramWithProposal().flatMap: pid =>
      notAuthorized(ngoCa)(setTacFeedbackAs(ngoCa, pid, feedback(US)))

  test("ngo can't set TAC feedback for a category that is not a partner"):
    createProgramWithProposal().flatMap: pid =>
      notAuthorized(ngoUs)(setTacFeedbackAs(ngoUs, pid, feedback(TimeAccountingCategory.GT)))

  test("ngo can't set TAC feedback for a program it cannot read"):
    // CA has a 0% split, so the CA NGO cannot see the program.
    createProgramWithProposal(List((Partner.US, 100), (Partner.CA, 0))).flatMap: pid =>
      notAuthorized(ngoCa)(setTacFeedbackAs(ngoCa, pid, feedback(CA))) *>
      setTacFeedbackAs(ngoUs, pid, feedback(US)).void

  test("staff, admin and service can set TAC feedback for any partner"):
    createProgramWithProposal(List((Partner.AR, 25), (Partner.BR, 25), (Partner.CL, 25), (Partner.US, 25))).flatMap: pid =>
      List((staff, TimeAccountingCategory.AR), (admin, TimeAccountingCategory.BR), (service, TimeAccountingCategory.CL)).traverse_ : (user, category) =>
        setTacFeedbackAs(user, pid, feedback(category))

  def noSplitMessage(pid: Program.Id, partner: String): String =
    s"Partner $partner has no time split in the proposal for program $pid, nor is it the PI's partner in a Fast Turnaround or Poor Weather proposal, so it cannot provide TAC feedback."

  test("TAC feedback requires a non-zero time split for the partner"):
    createProgramWithProposal(List((Partner.US, 100), (Partner.CA, 0))).flatMap: pid =>
      invalid(setTacFeedbackAs(staff, pid, feedback(CA)))(noSplitMessage(pid, "CA")) *>                         // a 0% split
      invalid(setTacFeedbackAs(staff, pid, feedback(TimeAccountingCategory.AR)))(noSplitMessage(pid, "AR")) *>  // no split at all
      setTacFeedbackAs(staff, pid, feedback(US)).void

  test("TAC feedback without a time split is allowed for the PI's partner in Fast Turnaround and Poor Weather proposals"):
    // These proposals have no splits: their time comes from the PI's partner.
    List(
      (GeminiCallForProposalsType.FastTurnaround, "fastTurnaround: {}"),
      (GeminiCallForProposalsType.PoorWeather,    "poorWeather: {}")
    ).traverse_ : (callType, props) =>
      createProgramWithCall(callType, props, PartnerLink.HasGeminiPartner(Partner.CA)).flatMap: pid =>
        setTacFeedbackAs(staff, pid, feedback(CA)) *>
        invalid(setTacFeedbackAs(staff, pid, feedback(US)))(noSplitMessage(pid, "US"))

  test("ngo can set TAC feedback for its PI's Fast Turnaround and Poor Weather proposals"):
    List(
      (GeminiCallForProposalsType.FastTurnaround, "fastTurnaround: {}"),
      (GeminiCallForProposalsType.PoorWeather,    "poorWeather: {}")
    ).traverse_ : (callType, props) =>
      createProgramWithCall(callType, props, PartnerLink.HasGeminiPartner(Partner.CA)).flatMap: pid =>
        setTacFeedbackAs(ngoCa, pid, feedback(CA)) *>
        notAuthorized(ngoUs)(setTacFeedbackAs(ngoUs, pid, feedback(US)))

  test("TAC feedback without a time split is refused for the PI's partner in other proposals"):
    // A CA PI may request all of the time from the US, leaving CA with none.
    val props = "queue: { considerForBand3: DO_NOT_CONSIDER, partnerSplits: [{ partner: US, percent: 100 }] }"
    createProgramWithCall(RegularSemester, props, PartnerLink.HasGeminiPartner(Partner.CA)).flatMap: pid =>
      invalid(setTacFeedbackAs(staff, pid, feedback(CA)))(noSplitMessage(pid, "CA")) *>
      setTacFeedbackAs(staff, pid, feedback(US)).void

  test("ngo can't set TAC feedback without a time split, even with an allocation"):
    // The allocation makes the program visible to the CA NGO, but CA requested no time.
    for
      pid <- createProgramWithProposal(List((Partner.US, 100)))
      _   <- setOneAllocationAs(staff, pid, CA, ScienceBand.Band1, TimeSpan.fromHours(1).get)
      _   <- invalid(setTacFeedbackAs(ngoCa, pid, feedback(CA)))(noSplitMessage(pid, "CA"))
    yield ()

  test("TAC feedback requires a time split in a recreated proposal"):
    // The deleted proposal's splits must not carry over to its replacement,
    // here a Demo Science proposal, which has none.
    for
      pid <- createProgramWithProposal(List((Partner.US, 100)))
      _   <- deleteProposal(staff, pid).map(assert(_))
      _   <- addProposal(pi, pid, callProps = "demoScience: { minPercentTime: 0 }".some)
      _   <- submitted(pid)
      _   <- invalid(setTacFeedbackAs(staff, pid, feedback(US)))(noSplitMessage(pid, "US"))
    yield ()

  test("TAC feedback can be set in a proposal recreated with the same split"):
    for
      pid <- createProgramWithProposal(List((Partner.US, 100)))
      _   <- deleteProposal(staff, pid).map(assert(_))
      _   <- addProposal(pi, pid, callProps = "queue: { considerForBand3: DO_NOT_CONSIDER, partnerSplits: [{ partner: US, percent: 100 }] }".some)
      _   <- submitted(pid)
      _   <- setTacFeedbackAs(staff, pid, feedback(US))
    yield ()

  test("TAC feedback requires a proposal"):
    createProgramAs(pi).flatMap: pid =>
      invalid(setTacFeedbackAs(staff, pid, feedback(US)))(s"Program $pid has no proposal, so its TAC feedback cannot be changed.")

  test("TAC feedback can only be changed while the proposal is submitted"):
    def refused(pid: Program.Id, status: ProposalStatus, label: String): IO[Unit] =
      setProposalStatusDirectly(pid, status) *>
        invalid(setTacFeedbackAs(staff, pid, feedback(US)))(s"TAC feedback for program $pid cannot be changed while its proposal status is $label.")

    for
      pid <- createProgramWithProposal()
      _   <- setTacFeedbackAs(staff, pid, feedback(US))
      _   <- refused(pid, ProposalStatus.NotSubmitted, "NOT_SUBMITTED")
      _   <- refused(pid, ProposalStatus.Accepted,     "ACCEPTED")
      _   <- refused(pid, ProposalStatus.NotAccepted,  "NOT_ACCEPTED")
    yield ()

  test("GT and LTP can set TAC feedback for any proposal"):
    createProgramWithProposal().flatMap: pid =>
      setTacFeedbackAs(staff, pid, feedback(TimeAccountingCategory.GT)) *>
      setTacFeedbackAs(staff, pid, feedback(TimeAccountingCategory.LTP)).void

  test("CAL, ENG and CFHT can't set TAC feedback"):
    createProgramWithProposal().flatMap: pid =>
      List(TimeAccountingCategory.CAL, TimeAccountingCategory.ENG, TimeAccountingCategory.CFHT).traverse_ : category =>
        invalid(setTacFeedbackAs(staff, pid, feedback(category)))(s"Time accounting category ${category.tag.toUpperCase} cannot provide TAC feedback.")

  test("JP and KECK require a proposal requesting time on behalf of Subaru or Keck"):
    def notOnBehalf(pid: Program.Id, category: TimeAccountingCategory, community: String): IO[Unit] =
      invalid(setTacFeedbackAs(staff, pid, feedback(category)))(
        s"Time accounting category ${category.tag.toUpperCase} can only provide TAC feedback for a proposal requesting time on behalf of $community, which is not the case for program $pid."
      )

    def exchangeProps(partner: String): String =
      s"queue: { considerForBand3: DO_NOT_CONSIDER, exchangePartner: $partner }"

    for
      subaru <- createProgramWithCall(RegularSemester, exchangeProps("SUBARU"), PartnerLink.HasExchangePartner(ExchangePartner.Subaru))
      keck   <- createProgramWithCall(RegularSemester, exchangeProps("KECK"),   PartnerLink.HasExchangePartner(ExchangePartner.Keck))
      _      <- setTacFeedbackAs(staff, subaru, feedback(TimeAccountingCategory.JP))
      _      <- notOnBehalf(subaru, TimeAccountingCategory.KECK, "Keck")
      _      <- setTacFeedbackAs(staff, keck, feedback(TimeAccountingCategory.KECK))
      _      <- notOnBehalf(keck, TimeAccountingCategory.JP, "Subaru")
    yield ()

  test("JP requires the proposal, not just its PI, to be on behalf of Subaru"):
    createProgramWithCall(RegularSemester, "queue: { considerForBand3: DO_NOT_CONSIDER }", PartnerLink.HasExchangePartner(ExchangePartner.Subaru)).flatMap: pid =>
      invalid(setTacFeedbackAs(staff, pid, feedback(TimeAccountingCategory.JP)))(
        s"Time accounting category JP can only provide TAC feedback for a proposal requesting time on behalf of Subaru, which is not the case for program $pid."
      )

  test("DD, DS, LP and SV require a call for proposals of the matching type"):
    def wrongCall(pid: Program.Id, category: TimeAccountingCategory, callTitle: String): IO[Unit] =
      invalid(setTacFeedbackAs(staff, pid, feedback(category)))(
        s"Time accounting category ${category.tag.toUpperCase} can only provide TAC feedback for a proposal to a $callTitle call for proposals, which is not the case for program $pid."
      )

    for
      dd <- createProgramWithCall(GeminiCallForProposalsType.DirectorsTime, "directorsTime: {}")
      ds <- createProgramWithCall(GeminiCallForProposalsType.DemoScience,   "demoScience: { minPercentTime: 0 }")
      rs <- createProgramWithCall(RegularSemester,                          "queue: { considerForBand3: DO_NOT_CONSIDER }")
      _  <- setTacFeedbackAs(staff, dd, feedback(TimeAccountingCategory.DD))
      _  <- wrongCall(dd, TimeAccountingCategory.DS, "Demo Science")
      _  <- setTacFeedbackAs(staff, ds, feedback(TimeAccountingCategory.DS))
      _  <- wrongCall(ds, TimeAccountingCategory.DD, "Director's Time")
      _  <- wrongCall(rs, TimeAccountingCategory.LP, "Large Program")
      _  <- wrongCall(rs, TimeAccountingCategory.SV, "System Verification")
    yield ()

  test("new TAC feedback requires a decision and ranking"):
    createProgramWithProposal().flatMap: pid =>
      invalid(setTacFeedbackAs(staff, pid, partial(US).copy(decision = TacPartnerDecision.Rejected.some)))(
        "ranking must be specified when creating TAC feedback for time accounting category US."
      )

  test("new TAC feedback with no fields at all is rejected"):
    createProgramWithProposal().flatMap: pid =>
      invalid(setTacFeedbackAs(staff, pid, partial(US)))(
        "decision, ranking must be specified when creating TAC feedback for time accounting category US."
      )

  test("TAC feedback may be updated one field at a time"):
    createProgramWithProposal().flatMap: pid =>
      for
        _ <- setTacFeedbackAs(staff, pid, feedback(US))
        a <- setTacFeedbackAs(staff, pid, partial(US).copy(decision = TacPartnerDecision.Rejected.some))
        b <- setTacFeedbackAs(staff, pid, partial(US).copy(ranking = PosBigDecimal.unsafeFrom(BigDecimal(3)).some))
        c <- setTacFeedbackAs(staff, pid, partial(US).copy(recommendedTimes = recommendedTimes(ScienceBand.Band1 -> BigDecimal("0.5")).some))
        d <- setTacFeedbackAs(staff, pid, partial(US).copy(comments = Nullable.NonNull(NonEmptyString.unsafeFrom("Revised."))))
        e <- setTacFeedbackAs(staff, pid, partial(US))
      yield
        assertEquals(a, setResult(feedbackJson("US", "REJECTED", BigDecimal("1.5"), band1("2.000000"), "Great science.")))
        assertEquals(b, setResult(feedbackJson("US", "REJECTED", BigDecimal(3),     band1("2.000000"), "Great science.")))
        assertEquals(c, setResult(feedbackJson("US", "REJECTED", BigDecimal(3),     band1("0.500000"), "Great science.")))
        assertEquals(d, setResult(feedbackJson("US", "REJECTED", BigDecimal(3),     band1("0.500000"), "Revised.")))
        assertEquals(e, d)

  test("comments are optional when creating TAC feedback"):
    createProgramWithProposal().flatMap: pid =>
      setTacFeedbackAs(staff, pid, feedback(US).copy(comments = Nullable.Absent)).map: js =>
        assertEquals(js, setResult(feedbackJson("US", "ACCEPTED", BigDecimal("1.5"), band1("2.000000"), none)))

  test("explicit null comments remove existing comments"):
    createProgramWithProposal().flatMap: pid =>
      for
        _ <- setTacFeedbackAs(staff, pid, feedback(US))
        a <- setTacFeedbackAs(staff, pid, partial(US).copy(comments = Nullable.Null))
        b <- setTacFeedbackAs(staff, pid, partial(US).copy(decision = TacPartnerDecision.Rejected.some))
        c <- setTacFeedbackAs(staff, pid, partial(US).copy(comments = Nullable.NonNull(NonEmptyString.unsafeFrom("Back again."))))
      yield
        assertEquals(a, setResult(feedbackJson("US", "ACCEPTED", BigDecimal("1.5"), band1("2.000000"), none)))
        assertEquals(b, setResult(feedbackJson("US", "REJECTED", BigDecimal("1.5"), band1("2.000000"), none)))
        assertEquals(c, setResult(feedbackJson("US", "REJECTED", BigDecimal("1.5"), band1("2.000000"), "Back again.")))

  test("recommended times are optional when creating TAC feedback"):
    createProgramWithProposal().flatMap: pid =>
      setTacFeedbackAs(staff, pid, feedback(US).copy(recommendedTimes = none)).map: js =>
        assertEquals(js, setResult(feedbackJson("US", "ACCEPTED", BigDecimal("1.5"), Nil, "Great science.")))

  test("recommended times are sorted by band"):
    createProgramWithProposal().flatMap: pid =>
      val times = List(ScienceBand.Band3 -> BigDecimal(3), ScienceBand.Band1 -> BigDecimal(1), ScienceBand.Band2 -> BigDecimal(2))
      setTacFeedbackAs(staff, pid, feedback(US, times = times)).map: js =>
        val expected = List(
          "BAND1" -> BigDecimal("1.000000"),
          "BAND2" -> BigDecimal("2.000000"),
          "BAND3" -> BigDecimal("3.000000")
        )
        assertEquals(js, setResult(feedbackJson("US", "ACCEPTED", BigDecimal("1.5"), expected, "Great science.")))

  test("supplied recommended times replace the existing ones"):
    createProgramWithProposal().flatMap: pid =>
      val initial = List(ScienceBand.Band1 -> BigDecimal(2), ScienceBand.Band2 -> BigDecimal(1))
      for
        _ <- setTacFeedbackAs(staff, pid, feedback(US, times = initial))
        // Remove BAND1, change BAND2 and add BAND3 in one call.
        a <- setTacFeedbackAs(staff, pid, partial(US).copy(recommendedTimes = recommendedTimes(ScienceBand.Band3 -> BigDecimal("0.5"), ScienceBand.Band2 -> BigDecimal(3)).some))
        // Omitting the list keeps it.
        b <- setTacFeedbackAs(staff, pid, partial(US).copy(decision = TacPartnerDecision.Rejected.some))
        // An empty list removes them all.
        c <- setTacFeedbackAs(staff, pid, partial(US).copy(recommendedTimes = Nil.some))
      yield
        val replaced = List("BAND2" -> BigDecimal("3.000000"), "BAND3" -> BigDecimal("0.500000"))
        assertEquals(a, setResult(feedbackJson("US", "ACCEPTED", BigDecimal("1.5"), replaced, "Great science.")))
        assertEquals(b, setResult(feedbackJson("US", "REJECTED", BigDecimal("1.5"), replaced, "Great science.")))
        assertEquals(c, setResult(feedbackJson("US", "REJECTED", BigDecimal("1.5"), Nil,      "Great science.")))

  test("recommended times set the allocations to match"):
    createProgramWithProposal().flatMap: pid =>
      val times = List(ScienceBand.Band1 -> BigDecimal(2), ScienceBand.Band2 -> BigDecimal(1))
      setTacFeedbackAs(staff, pid, feedback(US, times = times)) *>
      allocations(pid).map: as =>
        assertEquals(as, List(("US", "BAND1", BigDecimal("2.000000")), ("US", "BAND2", BigDecimal("1.000000"))))

  test("supplying a recommended time again resets the allocated duration"):
    createProgramWithProposal().flatMap: pid =>
      for
        _  <- setTacFeedbackAs(staff, pid, feedback(US))
        _  <- setAllocationsAs(staff, pid, List(AllocationInput(US, ScienceBand.Band1, TimeSpan.fromHours(5).get)))
        a  <- allocations(pid)
        _  <- setTacFeedbackAs(staff, pid, partial(US).copy(recommendedTimes = recommendedTimes(ScienceBand.Band1 -> BigDecimal(2)).some))
        b  <- allocations(pid)
        fb <- query(staff, selectTacFeedback(pid))
      yield
        assertEquals(a, List(("US", "BAND1", BigDecimal("5.000000"))))
        assertEquals(b, List(("US", "BAND1", BigDecimal("2.000000"))))
        assertEquals(fb, json"""{ "program": { "tacFeedback": [ ${feedbackJson("US", "ACCEPTED", BigDecimal("1.5"), band1("2.000000"), "Great science.")} ] } }""")

  test("removing a recommended time removes its allocation"):
    createProgramWithProposal().flatMap: pid =>
      val times = List(ScienceBand.Band1 -> BigDecimal(2), ScienceBand.Band2 -> BigDecimal(1))
      for
        _ <- setOneAllocationAs(staff, pid, CA, ScienceBand.Band3, TimeSpan.fromHours(4).get)
        _ <- setTacFeedbackAs(staff, pid, feedback(US, times = times))
        _ <- setTacFeedbackAs(staff, pid, partial(US).copy(recommendedTimes = recommendedTimes(ScienceBand.Band2 -> BigDecimal(1)).some))
        a <- allocations(pid)
        _ <- setTacFeedbackAs(staff, pid, partial(US).copy(recommendedTimes = Nil.some))
        b <- allocations(pid)
      yield
        // The CA allocation carries no recommended time, so it stays.
        assertEquals(a, List(("US", "BAND2", BigDecimal("1.000000")), ("CA", "BAND3", BigDecimal("4.000000"))))
        assertEquals(b, List(("CA", "BAND3", BigDecimal("4.000000"))))

  def scienceBand(oid: Observation.Id): IO[Option[ScienceBand]] =
    query(staff, s"""query { observation(observationId: "$oid") { scienceBand } }""").flatMap: js =>
      js.hcursor
        .downFields("observation", "scienceBand")
        .as[Option[ScienceBand]]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  test("a recommended time in a single band gives it to observations without one"):
    for
      pid <- createProgramWithProposal()
      oid <- createObservationAs(pi, pid)
      _   <- setTacFeedbackAs(staff, pid, feedback(US))
      b   <- scienceBand(oid)
    yield assertEquals(b, ScienceBand.Band1.some)

  test("removing a recommended time warns about observations in its band"):
    val times = List(ScienceBand.Band1 -> BigDecimal(2), ScienceBand.Band2 -> BigDecimal(1))
    for
      pid <- createProgramWithProposal()
      _   <- setTacFeedbackAs(staff, pid, feedback(US, times = times))
      oid <- createObservationAs(pi, pid)
      _   <- setScienceBandAs(pi, oid, ScienceBand.Band2.some)
      _   <- expectIor(
               user     = staff,
               query    = s"""
                 mutation {
                   setTacFeedback(input: {
                     programId: "$pid"
                     tacFeedback: {
                       timeAccountingCategory: US
                       recommendedTimes: [ { band: BAND1, time: { hours: 2 } } ]
                     }
                   }) {
                     tacFeedback { timeAccountingCategory }
                   }
                 }
               """,
               expected = Ior.both(
                 List(s"The following observations now have a science band for which no time is allocated: $oid"),
                 json"""{ "setTacFeedback": { "tacFeedback": { "timeAccountingCategory": "US" } } }"""
               )
             )
    yield ()

  test("recommended times may not repeat a band"):
    createProgramWithProposal().flatMap: pid =>
      expectOdbError(
        user     = staff,
        query    = s"""
          mutation {
            setTacFeedback(input: {
              programId: "$pid"
              tacFeedback: {
                timeAccountingCategory: US
                decision: ACCEPTED
                ranking: 1
                recommendedTimes: [
                  { band: BAND2, time: { hours: 1 } },
                  { band: BAND1, time: { hours: 1 } },
                  { band: BAND2, time: { hours: 2 } }
                ]
              }
            }) {
              tacFeedback { timeAccountingCategory }
            }
          }
        """,
        expected = {
          case OdbError.InvalidArgument(Some(m)) if m.contains("duplicate 'recommendedTimes band' specified: BAND2") => ()
        }
      )

  test("recommended times must be positive"):
    def refused(pid: Program.Id, hours: String)(matches: String => Boolean): IO[Unit] =
      expectOdbError(
        user     = staff,
        query    = s"""
          mutation {
            setTacFeedback(input: {
              programId: "$pid"
              tacFeedback: {
                timeAccountingCategory: US
                decision: ACCEPTED
                ranking: 1
                recommendedTimes: [ { band: BAND1, time: { hours: $hours } } ]
              }
            }) {
              tacFeedback { timeAccountingCategory }
            }
          }
        """,
        expected = {
          case OdbError.InvalidArgument(Some(m)) if matches(m) => ()
        }
      )

    createProgramWithProposal().flatMap: pid =>
      refused(pid, "0")(_.contains("Recommended times must be greater than zero.")) *>
      refused(pid, "-1")(_.startsWith("Argument 'input.tacFeedback.recommendedTimes")) *>
      allocations(pid).map(assertEquals(_, Nil))

  test("explicit null recommended times are rejected"):
    createProgramWithProposal().flatMap: pid =>
      setTacFeedbackAs(staff, pid, feedback(US)) *>
      expect(
        user     = staff,
        query    = s"""
          mutation {
            setTacFeedback(input: {
              programId: "$pid"
              tacFeedback: { timeAccountingCategory: US, recommendedTimes: null }
            }) {
              tacFeedback { timeAccountingCategory }
            }
          }
        """,
        expected = List("Argument 'input.tacFeedback.recommendedTimes' is invalid: cannot be null").asLeft
      )

  test("TAC feedback can be read by anyone who can read the program, ordered by category"):
    createProgramWithProposal().flatMap: pid =>
      setTacFeedbackAs(ngoUs, pid, feedback(US, comments = "From the US.")) *>
      setTacFeedbackAs(ngoCa, pid, feedback(CA, TacPartnerDecision.Rejected, BigDecimal(4), List(ScienceBand.Band1 -> BigDecimal(1)), "From Canada.")) *>
      setTacFeedbackAs(staff, pid, feedback(TimeAccountingCategory.GT, times = Nil, comments = "Guaranteed.")) *>
      List(pi, ngoCa, ngoUs, staff).traverse_ : user =>
        expect(
          user     = user,
          query    = selectTacFeedback(pid),
          expected = json"""
            {
              "program": {
                "tacFeedback": [
                  ${feedbackJson("CA", "REJECTED", BigDecimal(4), band1("1.000000"), "From Canada.")},
                  ${feedbackJson("GT", "ACCEPTED", BigDecimal("1.5"), Nil, "Guaranteed.")},
                  ${feedbackJson("US", "ACCEPTED", BigDecimal("1.5"), band1("2.000000"), "From the US.")}
                ]
              }
            }
          """.asRight
        )

  test("a program without TAC feedback has an empty list"):
    createProgramWithProposal().flatMap: pid =>
      expect(pi, selectTacFeedback(pid), json"""{ "program": { "tacFeedback": [] } }""".asRight)

  test("explicit nulls are rejected"):
    createProgramWithProposal().flatMap: pid =>
      setTacFeedbackAs(staff, pid, feedback(US)) *>
      expect(
        user     = staff,
        query    = s"""
          mutation {
            setTacFeedback(input: {
              programId: "$pid"
              tacFeedback: { timeAccountingCategory: US, ranking: null }
            }) {
              tacFeedback { timeAccountingCategory }
            }
          }
        """,
        expected = List("Argument 'input.tacFeedback.ranking' is invalid: cannot be null").asLeft
      )

  test("ranking must be positive"):
    createProgramWithProposal().flatMap: pid =>
      expectOdbError(
        user     = staff,
        query    = s"""
          mutation {
            setTacFeedback(input: {
              programId: "$pid"
              tacFeedback: { timeAccountingCategory: US, ranking: 0 }
            }) {
              tacFeedback { timeAccountingCategory }
            }
          }
        """,
        expected = {
          case OdbError.InvalidArgument(Some(m)) if m.startsWith("Argument 'input.tacFeedback.ranking' is invalid") => ()
        }
      )

  test("comments may not be empty"):
    createProgramWithProposal().flatMap: pid =>
      expectOdbError(
        user     = staff,
        query    = s"""
          mutation {
            setTacFeedback(input: {
              programId: "$pid"
              tacFeedback: { timeAccountingCategory: US, comments: "" }
            }) {
              tacFeedback { timeAccountingCategory }
            }
          }
        """,
        expected = {
          case OdbError.InvalidArgument(Some(m)) if m.startsWith("Argument 'input.tacFeedback.comments' is invalid") => ()
        }
      )

  test("set TAC feedback with a proposal reference"):
    for
      pid <- createSubmittableProposal
      ref <- submitProposal(pi, pid)
      _   <- expect(
               user     = staff,
               query    = s"""
                 mutation {
                   setTacFeedback(input: {
                     proposalReference: "${ref.label}"
                     tacFeedback: ${tacFeedbackInputString(feedback(US))}
                   }) {
                     tacFeedback { timeAccountingCategory decision }
                   }
                 }
               """,
               expected = json"""{ "setTacFeedback": { "tacFeedback": { "timeAccountingCategory": "US", "decision": "ACCEPTED" } } }""".asRight
             )
    yield ()

  test("ngo learns nothing about a reference to a program it cannot read"):
    // The US-only proposal is invisible to the CA NGO.  Pairing its reference
    // with another program id must not reveal the referenced program's id.
    for
      pid   <- createSubmittableProposal
      ref   <- submitProposal(pi, pid)
      other <- createProgramWithProposal()
      _     <- notAuthorized(ngoCa):
                 query(
                   user  = ngoCa,
                   query = s"""
                     mutation {
                       setTacFeedback(input: {
                         programId: "$other"
                         proposalReference: "${ref.label}"
                         tacFeedback: ${tacFeedbackInputString(feedback(CA))}
                       }) {
                         tacFeedback { timeAccountingCategory }
                       }
                     }
                   """
                 )
    yield ()

  test("TAC feedback for an accepted proposal can't be set by program reference"):
    // Program references are assigned on acceptance, after which TAC feedback
    // is locked.
    for
      pid <- createSubmittableProposal
      _   <- submitProposal(pi, pid)
      ref <- acceptProposal(staff, pid)
      _   <- expectOdbError(
               user     = staff,
               query    = s"""
                 mutation {
                   setTacFeedback(input: {
                     programReference: "${ref.get.label}"
                     tacFeedback: ${tacFeedbackInputString(feedback(US))}
                   }) {
                     tacFeedback { timeAccountingCategory decision }
                   }
                 }
               """,
               expected = {
                 case OdbError.InvalidArgument(Some(m)) if m == s"TAC feedback for program $pid cannot be changed while its proposal status is ACCEPTED." => ()
               }
             )
    yield ()
