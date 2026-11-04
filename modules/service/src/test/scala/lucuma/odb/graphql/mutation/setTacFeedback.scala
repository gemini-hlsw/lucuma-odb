// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.GeminiCallForProposalsType.RegularSemester
import lucuma.core.enums.Partner
import lucuma.core.enums.TacPartnerDecision
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.input.TacFeedbackInput

class setTacFeedback extends OdbSuite with query.ObservingModeSetupOperations:

  val guest   = TestUsers.guest(nextId)
  val pi      = TestUsers.Standard.pi(nextId, nextId)
  val ngoCa   = TestUsers.Standard.ngo(nextId, nextId, Partner.CA)
  val ngoUs   = TestUsers.Standard.ngo(nextId, nextId, Partner.US)
  val staff   = TestUsers.Standard.staff(nextId, nextId)
  val admin   = TestUsers.Standard.admin(nextId, nextId)
  val service = TestUsers.service(nextId)

  lazy val validUsers = List(guest, pi, ngoCa, ngoUs, staff, admin, service)

  def feedback(
    partner:  Partner,
    decision: TacPartnerDecision = TacPartnerDecision.Accepted,
    ranking:  BigDecimal         = BigDecimal("1.5"),
    hours:    BigDecimal         = BigDecimal(2),
    comments: String             = "Great science."
  ): TacFeedbackInput =
    TacFeedbackInput(
      partner,
      decision.some,
      PosBigDecimal.unsafeFrom(ranking).some,
      TimeSpan.fromHours(hours).get.some,
      Nullable.NonNull(NonEmptyString.unsafeFrom(comments))
    )

  def partial(partner: Partner): TacFeedbackInput =
    TacFeedbackInput(partner, none, none, none, Nullable.Absent)

  def feedbackJson(
    partner:  String,
    decision: String,
    ranking:  BigDecimal,
    hours:    BigDecimal,
    comments: String
  ): Json =
    feedbackJson(partner, decision, ranking, hours, comments.some)

  def feedbackJson(
    partner:  String,
    decision: String,
    ranking:  BigDecimal,
    hours:    BigDecimal,
    comments: Option[String]
  ): Json =
    json"""
      {
        "partner": $partner,
        "decision": $decision,
        "ranking": $ranking,
        "recommendedTime": { "hours": $hours },
        "comments": $comments
      }
    """

  def setResult(f: Json): Json =
    json"""{ "setTacFeedback": { "tacFeedback": $f } }"""

  // A program whose proposal requests time from the US (70%) and CA (30%).
  def createProgramWithProposal(splits: List[(Partner, Int)] = List((Partner.US, 70), (Partner.CA, 30))): IO[Program.Id] =
    for
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = splits)
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

  def selectTacFeedback(pid: Program.Id): String =
    s"""
      query {
        program(programId: "$pid") {
          tacFeedback {
            partner
            decision
            ranking
            recommendedTime { hours }
            comments
          }
        }
      }
    """

  test("guest and pi can't set TAC feedback"):
    createProgramWithProposal().flatMap: pid =>
      List(guest, pi).traverse_ : user =>
        notAuthorized(user)(setTacFeedbackAs(user, pid, feedback(Partner.US)))

  test("ngo can set TAC feedback for its own partner"):
    createProgramWithProposal().flatMap: pid =>
      setTacFeedbackAs(ngoCa, pid, feedback(Partner.CA)).map: js =>
        assertEquals(js, setResult(feedbackJson("CA", "ACCEPTED", BigDecimal("1.5"), BigDecimal("2.000000"), "Great science.")))

  test("ngo can't set TAC feedback for another partner"):
    createProgramWithProposal().flatMap: pid =>
      notAuthorized(ngoCa)(setTacFeedbackAs(ngoCa, pid, feedback(Partner.US)))

  test("ngo can't set TAC feedback for a program it cannot read"):
    // CA has a 0% split, so the CA NGO cannot see the program.
    createProgramWithProposal(List((Partner.US, 100), (Partner.CA, 0))).flatMap: pid =>
      notAuthorized(ngoCa)(setTacFeedbackAs(ngoCa, pid, feedback(Partner.CA))) *>
      setTacFeedbackAs(ngoUs, pid, feedback(Partner.US)).void

  test("staff, admin and service can set TAC feedback for any partner"):
    createProgramWithProposal(List((Partner.AR, 25), (Partner.BR, 25), (Partner.CL, 25), (Partner.US, 25))).flatMap: pid =>
      List((staff, Partner.AR), (admin, Partner.BR), (service, Partner.CL)).traverse_ : (user, partner) =>
        setTacFeedbackAs(user, pid, feedback(partner))

  test("TAC feedback requires a non-zero time split for the partner"):
    def noSplit(pid: Program.Id, partner: Partner): IO[Unit] =
      interceptOdbError(setTacFeedbackAs(staff, pid, feedback(partner))):
        case OdbError.InvalidArgument(Some(m)) =>
          assertEquals(m, s"Partner ${partner.tag.toUpperCase} has no time split in the proposal for program $pid, so it cannot provide TAC feedback.")

    createProgramWithProposal(List((Partner.US, 100), (Partner.CA, 0))).flatMap: pid =>
      noSplit(pid, Partner.CA) *>    // a 0% split
      noSplit(pid, Partner.AR) *>    // no split at all
      setTacFeedbackAs(staff, pid, feedback(Partner.US)).void

  test("ngo can't set TAC feedback without a time split, even with an allocation"):
    // The allocation makes the program visible to the CA NGO, but CA requested no time.
    for
      pid <- createProgramWithProposal(List((Partner.US, 100)))
      _   <- setOneAllocationAs(staff, pid, lucuma.core.enums.TimeAccountingCategory.CA, lucuma.core.enums.ScienceBand.Band1, TimeSpan.fromHours(1).get)
      _   <- interceptOdbError(setTacFeedbackAs(ngoCa, pid, feedback(Partner.CA))):
               case OdbError.InvalidArgument(Some(m)) =>
                 assert(m.startsWith("Partner CA has no time split"), m)
    yield ()

  test("TAC feedback requires a proposal"):
    createProgramAs(pi).flatMap: pid =>
      interceptOdbError(setTacFeedbackAs(staff, pid, feedback(Partner.US))):
        case OdbError.InvalidArgument(Some(m)) =>
          assertEquals(m, s"Program $pid has no proposal, so TAC feedback cannot be set.")

  test("new TAC feedback requires a decision, ranking and recommended time"):
    createProgramWithProposal().flatMap: pid =>
      interceptOdbError(setTacFeedbackAs(staff, pid, partial(Partner.US).copy(decision = TacPartnerDecision.Rejected.some))):
        case OdbError.InvalidArgument(Some(m)) =>
          assertEquals(m, "ranking, recommendedTime must be specified when creating TAC feedback for partner US.")

  test("new TAC feedback with no fields at all is rejected"):
    createProgramWithProposal().flatMap: pid =>
      interceptOdbError(setTacFeedbackAs(staff, pid, partial(Partner.US))):
        case OdbError.InvalidArgument(Some(m)) =>
          assertEquals(m, "decision, ranking, recommendedTime must be specified when creating TAC feedback for partner US.")

  test("TAC feedback may be updated one field at a time"):
    createProgramWithProposal().flatMap: pid =>
      for
        _ <- setTacFeedbackAs(staff, pid, feedback(Partner.US))
        a <- setTacFeedbackAs(staff, pid, partial(Partner.US).copy(decision = TacPartnerDecision.Rejected.some))
        b <- setTacFeedbackAs(staff, pid, partial(Partner.US).copy(ranking = PosBigDecimal.unsafeFrom(BigDecimal(3)).some))
        c <- setTacFeedbackAs(staff, pid, partial(Partner.US).copy(recommendedTime = TimeSpan.fromHours(BigDecimal("0.5")).get.some))
        d <- setTacFeedbackAs(staff, pid, partial(Partner.US).copy(comments = Nullable.NonNull(NonEmptyString.unsafeFrom("Revised."))))
        e <- setTacFeedbackAs(staff, pid, partial(Partner.US))
      yield
        assertEquals(a, setResult(feedbackJson("US", "REJECTED", BigDecimal("1.5"), BigDecimal("2.000000"), "Great science.")))
        assertEquals(b, setResult(feedbackJson("US", "REJECTED", BigDecimal(3),     BigDecimal("2.000000"), "Great science.")))
        assertEquals(c, setResult(feedbackJson("US", "REJECTED", BigDecimal(3),     BigDecimal("0.500000"), "Great science.")))
        assertEquals(d, setResult(feedbackJson("US", "REJECTED", BigDecimal(3),     BigDecimal("0.500000"), "Revised.")))
        assertEquals(e, d)

  test("comments are optional when creating TAC feedback"):
    createProgramWithProposal().flatMap: pid =>
      setTacFeedbackAs(staff, pid, feedback(Partner.US).copy(comments = Nullable.Absent)).map: js =>
        assertEquals(js, setResult(feedbackJson("US", "ACCEPTED", BigDecimal("1.5"), BigDecimal("2.000000"), none)))

  test("explicit null comments remove existing comments"):
    createProgramWithProposal().flatMap: pid =>
      for
        _ <- setTacFeedbackAs(staff, pid, feedback(Partner.US))
        a <- setTacFeedbackAs(staff, pid, partial(Partner.US).copy(comments = Nullable.Null))
        b <- setTacFeedbackAs(staff, pid, partial(Partner.US).copy(decision = TacPartnerDecision.Rejected.some))
        c <- setTacFeedbackAs(staff, pid, partial(Partner.US).copy(comments = Nullable.NonNull(NonEmptyString.unsafeFrom("Back again."))))
      yield
        assertEquals(a, setResult(feedbackJson("US", "ACCEPTED", BigDecimal("1.5"), BigDecimal("2.000000"), none)))
        assertEquals(b, setResult(feedbackJson("US", "REJECTED", BigDecimal("1.5"), BigDecimal("2.000000"), none)))
        assertEquals(c, setResult(feedbackJson("US", "REJECTED", BigDecimal("1.5"), BigDecimal("2.000000"), "Back again.")))

  test("TAC feedback can be read by anyone who can read the program, ordered by partner"):
    createProgramWithProposal().flatMap: pid =>
      setTacFeedbackAs(ngoUs, pid, feedback(Partner.US, comments = "From the US.")) *>
      setTacFeedbackAs(ngoCa, pid, feedback(Partner.CA, TacPartnerDecision.Rejected, BigDecimal(4), BigDecimal(1), "From Canada.")) *>
      List(pi, ngoCa, ngoUs, staff).traverse_ : user =>
        expect(
          user     = user,
          query    = selectTacFeedback(pid),
          expected = json"""
            {
              "program": {
                "tacFeedback": [
                  ${feedbackJson("CA", "REJECTED", BigDecimal(4), BigDecimal("1.000000"), "From Canada.")},
                  ${feedbackJson("US", "ACCEPTED", BigDecimal("1.5"), BigDecimal("2.000000"), "From the US.")}
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
      setTacFeedbackAs(staff, pid, feedback(Partner.US)) *>
      expect(
        user     = staff,
        query    = s"""
          mutation {
            setTacFeedback(input: {
              programId: "$pid"
              tacFeedback: { partner: US, ranking: null }
            }) {
              tacFeedback { partner }
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
              tacFeedback: { partner: US, ranking: 0 }
            }) {
              tacFeedback { partner }
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
              tacFeedback: { partner: US, comments: "" }
            }) {
              tacFeedback { partner }
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
                     tacFeedback: ${tacFeedbackInputString(feedback(Partner.US))}
                   }) {
                     tacFeedback { partner decision }
                   }
                 }
               """,
               expected = json"""{ "setTacFeedback": { "tacFeedback": { "partner": "US", "decision": "ACCEPTED" } } }""".asRight
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
                         tacFeedback: ${tacFeedbackInputString(feedback(Partner.CA))}
                       }) {
                         tacFeedback { partner }
                       }
                     }
                   """
                 )
    yield ()

  test("set TAC feedback with a program reference"):
    for
      pid <- createSubmittableProposal
      _   <- submitProposal(pi, pid)
      ref <- acceptProposal(staff, pid)
      _   <- expect(
               user     = staff,
               query    = s"""
                 mutation {
                   setTacFeedback(input: {
                     programReference: "${ref.get.label}"
                     tacFeedback: ${tacFeedbackInputString(feedback(Partner.US))}
                   }) {
                     tacFeedback { partner decision }
                   }
                 }
               """,
               expected = json"""{ "setTacFeedback": { "tacFeedback": { "partner": "US", "decision": "ACCEPTED" } } }""".asRight
             )
    yield ()
