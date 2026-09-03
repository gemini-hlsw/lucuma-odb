// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ProposalStatus
import lucuma.core.enums.ProposalSubmissionError
import lucuma.core.enums.ProposalSubmissionError.*
import lucuma.core.model.CallForProposals
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.service.ProposalService.error
import lucuma.odb.util.Codecs.program_id
import skunk.implicits.*

import java.time.Instant
import java.time.LocalDate

class setProposalStatus extends OdbSuite
  with ObservingModeSetupOperations {

  val pi       = TestUsers.Standard.pi(1, 101)
  val pi2      = TestUsers.Standard.pi(2, 102)
  val ngo      = TestUsers.Standard.ngo(3, 103, Partner.CA)
  val staff    = TestUsers.Standard.staff(4, 104)
  val admin    = TestUsers.Standard.admin(5, 105)
  val guest    = TestUsers.guest(6)

  val validUsers = List(pi, pi2, ngo, staff, admin, guest)

  val oneDay: TimeSpan     = TimeSpan.fromHours(24).get
  val yesterday: Timestamp = Timestamp.unsafeFromInstantTruncated(Instant.now) -| oneDay

  override val httpRequestHandler = invitationEmailRequestHandler

  private def addSubmissionPrerequisites(pid: Program.Id): IO[Unit] =
    addSubmissionPrerequisitesAs(pi, pid)


  test("✓ valid submission") {
    createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramWithNonPartnerPi(pi).flatMap { pid =>
        addProposal(pi, pid, cid.some) *>
        addSubmissionPrerequisites(pid) *>
        addPartnerSplits(pi, pid) *>
        addCoisAs(pi, pid) *>
        ensureNoEmailsForAddress(defaultPiEmail) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program { proposal { reference { label } } }
              }
            }
          """,
          expected =
            json"""
              {
                "setProposalStatus": {
                  "program": {
                    "proposal": {
                      "reference": { "label": "G-2025A-0001" }
                    }
                  }
                }
              }
            """.asRight
        ) *>
        ensureSomeQueuedEmailsForAddress(defaultPiEmail, 1)
      }
    }
  }

  // A Gemini call that offers Keck as an exchange partner, optionally with a
  // deadline override for it.
  private def createCallWithKeckExchangeAs(
    semester:         Semester,
    activeStart:      LocalDate,
    activeEnd:        LocalDate,
    deadlineOverride: Option[Timestamp] = none
  ): IO[CallForProposals.Id] =
    createGeminiCallForProposalsAs(
      staff,
      GeminiCallForProposalsType.RegularSemester,
      semester    = semester,
      activeStart = activeStart,
      activeEnd   = activeEnd,
      otherGemini = s"""
        exchangePartners: [
          {
            exchangePartner: KECK
            ${deadlineOverride.fold("")(ts => s"submissionDeadlineOverride: \"${ts.isoFormat}\"")}
          }
        ]
      """.some
    )

  test("✓ exchange partner submission") {
    // A Keck/Subaru PI requesting Gemini time: the proposal carries an exchange
    // partner instead of partner splits, and uses the call's default submission
    // deadline rather than any Gemini partner deadline.  Uses its own semester
    // so the assigned proposal reference doesn't perturb other tests' counts.
    createCallWithKeckExchangeAs(
      Semester.unsafeFromString("2026A"),
      LocalDate.parse("2026-02-01"),
      LocalDate.parse("2026-07-31")
    ).flatMap { cid =>
      createProgramWithPiAffiliation(
        pi,
        PartnerLink.HasExchangePartner(ExchangePartner.Keck)
      ).flatMap { pid =>
        addProposal(pi, pid, cid.some, "classical: { exchangePartner: KECK }".some) *>
        addSubmissionPrerequisites(pid) *>
        addCoisAs(pi, pid) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program {
                  id
                  proposalStatus
                  proposal {
                    gemini {
                      ... on Classical {
                        exchangePartner
                        partnerSplits { partner }
                      }
                    }
                  }
                }
              }
            }
          """,
          expected =
            json"""
              {
                "setProposalStatus": {
                  "program": {
                    "id": $pid,
                    "proposalStatus": "SUBMITTED",
                    "proposal": {
                      "gemini": {
                        "exchangePartner": "KECK",
                        "partnerSplits": []
                      }
                    }
                  }
                }
              }
            """.asRight
        )
      }
    }
  }

  test("⨯ exchange partner submission past the community's deadline override") {
    // The call is open (its default deadline is Timestamp.Max) but closed for the
    // Keck community, so the request is past its deadline.  The PI belongs to the
    // exchange community, so neither the Gemini partner nor the non-partner
    // deadline applies.
    createCallWithKeckExchangeAs(
      Semester.unsafeFromString("2026B"),
      LocalDate.parse("2026-08-01"),
      LocalDate.parse("2027-01-31"),
      deadlineOverride = yesterday.some
    ).flatMap { cid =>
      createProgramWithPiAffiliation(
        pi,
        PartnerLink.HasExchangePartner(ExchangePartner.Keck)
      ).flatMap { pid =>
        addProposal(pi, pid, cid.some, "classical: { exchangePartner: KECK }".some) *>
        addSubmissionPrerequisites(pid) *>
        addCoisAs(pi, pid) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) { program { id } }
            }
          """,
          expected = List(error.submissionError(ProposalSubmissionError.PastDeadline, pid).message).asLeft
        )
      }
    }
  }

  test("⨯ exchange partner the call does not offer") {
    // The call offers no exchange partners at all, so it cannot be asked for time
    // on behalf of the Keck community.
    createGeminiCallForProposalsAs(
      staff,
      GeminiCallForProposalsType.RegularSemester,
      semester    = Semester.unsafeFromString("2027A"),
      activeStart = LocalDate.parse("2027-02-01"),
      activeEnd   = LocalDate.parse("2027-07-31")
    ).flatMap { cid =>
      createProgramWithPiAffiliation(
        pi,
        PartnerLink.HasExchangePartner(ExchangePartner.Keck)
      ).flatMap { pid =>
        addProposal(pi, pid, cid.some, "classical: { exchangePartner: KECK }".some) *>
        addSubmissionPrerequisites(pid) *>
        addCoisAs(pi, pid) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) { program { id } }
            }
          """,
          expected =
            List(
              error.submissionError(ProposalSubmissionError.ExchangePartnerNotInCall, pid).message,
              error.submissionError(ProposalSubmissionError.MissingDeadline, pid).message
            ).asLeft
        )
      }
    }
  }

  test("⨯ undefined observation") {
    createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramWithNonPartnerPi(pi).flatMap { pid =>
        addProposal(pi, pid, cid.some) *>
        addSubmissionPrerequisites(pid) *>
        addPartnerSplits(pi, pid) *>
        addCoisAs(pi, pid) *>
        createObservationAs(pi, pid) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program { proposal { reference { label } } }
              }
            }
          """,
          expected = List(
            error.submissionError(ProposalSubmissionError.UndefinedObservations, pid).message
          ).asLeft
        )
      }
    }
  }

  test("⨯ missing two matching partners") {
    createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramWithNonPartnerPi(pi).flatMap { pid =>
        addProposal(pi, pid, cid.some) *>
        addSubmissionPrerequisites(pid) *>
        addPartnerSplits(pi, pid) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program { proposal { reference { label } } }
              }
            }
          """,
          expected = List(
            error.submissionError(ProposalSubmissionError.UnmatchedPartnerTime, pid).message
          ).asLeft
        )
      }
    }
  }

  // US is the default home for a proposal with no other affiliation, so unlike
  // every other partner a US share needs no investigator to back it.
  test("✓ US time requires no matching investigator") {
    createGeminiCallForProposalsAs(
      staff,
      GeminiCallForProposalsType.RegularSemester,
      semester    = Semester.unsafeFromString("2026B"),
      activeStart = LocalDate.parse("2026-08-01"),
      activeEnd   = LocalDate.parse("2027-01-31")
    ).flatMap { cid =>
      createProgramWithUsPi(pi).flatMap { pid =>
        addProposal(pi, pid, cid.some) *>
        addSubmissionPrerequisites(pid) *>
        addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100))) *>
        submitExpectingSuccess(pid)
      }
    }
  }

  test("⨯ missing partner splits (queue)") {
    createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramWithNonPartnerPi(pi).flatMap { pid =>
        addProposal(pi, pid, cid.some) *>
        addSubmissionPrerequisites(pid) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program { proposal { reference { label } } }
              }
            }
          """,
          expected =
            List(error.submissionError(ProposalSubmissionError.InvalidPartnerSplits, pid).message).asLeft
        )
      }
    }
  }

  test("✓ fast turnaround submission") {
    createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.FastTurnaround).flatMap { cid =>
      createProgramWithUsPi(pi).flatTap(setPiEducationalStatusAs(pi, _, EducationalStatus.PhD)).flatMap { pid =>
        addProposal(pi, pid, cid.some, "fastTurnaround: {}".some) *>
        addSubmissionPrerequisites(pid) *>
        addCoisAs(pi, pid, List(Partner.US)) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program { proposal { reference { label } } }
              }
            }
          """,
          expected =
            json"""
              {
                "setProposalStatus": {
                  "program": {
                    "proposal": {
                      "reference": { "label": "G-2025A-0002" }
                    }
                  }
                }
              }
            """.asRight

        )
      }
    }
  }

  test("⨯ update proposalStatus with no proposal") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                programId: "$pid"
                status: SUBMITTED
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(
            error.missingProposal(pid).message,
            error.submissionError(ProposalSubmissionError.MissingCfp, pid).message,
            error.submissionError(ProposalSubmissionError.MissingSemester, pid).message,
            error.submissionError(ProposalSubmissionError.MissingProposalType, pid).message,
            error.submissionError(ProposalSubmissionError.MissingCategory, pid).message,
            error.submissionError(ProposalSubmissionError.MissingPiEmail, pid).message,
            error.submissionError(ProposalSubmissionError.MissingTitle, pid).message,
            error.submissionError(ProposalSubmissionError.MissingAbstract, pid).message,
            error.submissionError(ProposalSubmissionError.UnspecifiedInvestigatorPartner, pid).message,
            error.submissionError(ProposalSubmissionError.NoDefinedObservations, pid).message,
            error.submissionError(ProposalSubmissionError.MissingDeadline, pid).message
          ))
      )
    }
  }

  test("⨯ pi update proposalStatus to unauthorized status") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid) >>
      addSubmissionPrerequisites(pid) >>
      expect(
        user = pi,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                programId: "$pid"
                status: ACCEPTED
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(error.notAuthorizedNew(pid, pi, ProposalStatus.Accepted).message))
      )
    }
  }

  test("⨯ guest submit") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid) >>
      addSubmissionPrerequisites(pid) >>
      // the non-guest requirement gets caught before it even gets to the service.
      expect(
        user = guest,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                status: SUBMITTED
                programId: "$pid"
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(OdbError.NotAuthorized(guest.id).message))
      )
    }
  }

  test("⨯ no CfP for proposal submission") {
    createProgramWithNonPartnerPi(pi).flatMap { pid =>
      addProposal(pi, pid) >>
      addSubmissionPrerequisites(pid) >>
      addPartnerSplits(pi, pid) >>
      addCoisAs(pi, pid) >>
      expect(
        user = pi,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                status: SUBMITTED
                programId: "$pid"
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(
            error.submissionError(ProposalSubmissionError.MissingCfp, pid).message,
            error.submissionError(ProposalSubmissionError.MissingSemester, pid).message,
            error.submissionError(ProposalSubmissionError.MissingDeadline, pid).message
          ))
      )
    }
  }

  test("⨯ non-science program type for proposal submission") {
    createProgramAs(pi).flatMap { pid =>
      setProgramReference(staff, pid, """calibration: { semester: "2025B", instrument: GMOS_SOUTH }""") >>
      expect(
        user = pi,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                status: SUBMITTED
                programId: "$pid"
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(error.invalidProgramType(pid, ProgramType.Calibration).message))
      )
    }
  }

  test("✓ edit proposal status (pi can set to SUBMITTED and back to NOT_SUBMITTED)") {

    def submit(pid: Program.Id): IO[Unit] =
      expect(
        user = pi,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                programId: "$pid"
                status: SUBMITTED,
              }
            ) {
              program {
                id
                proposalStatus
                proposal { reference { label } }
              }
            }
          }
        """,
        expected =
          json"""
            {
              "setProposalStatus" : {
                "program": {
                  "id" : $pid,
                  "proposalStatus": "SUBMITTED",
                  "proposal": { "reference": { "label": "G-2025A-0003" } }
                 }
              }
            }
          """.asRight
      )

    def recall(pid: Program.Id): IO[Unit] =
      expect(
        user = pi,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                status: NOT_SUBMITTED
                programId: "$pid"
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          json"""
            {
              "setProposalStatus" : {
                "program" : {
                  "id" : $pid,
                  "proposalStatus": "NOT_SUBMITTED"
                }
              }
            }
          """.asRight
      )

    def expected(pid: Program.Id): List[Json] =
        List(
          json"""
          {
            "c_user"                : ${pi.id},
            "c_mod_name"            : false,
            "c_new_name"            : null,
            "c_operation"           : "UPDATE",
            "c_program_id"          : $pid,
            "c_mod_existence"       : false,
            "c_new_existence"       : null,
            "c_mod_program_id"      : false,
            "c_new_program_id"      : null,
            "c_mod_proposal_status" : true,
            "c_new_proposal_status" : "submitted"
          }
          """,
          json"""
          {
            "c_user"                : ${pi.id},
            "c_mod_name"            : false,
            "c_new_name"            : null,
            "c_operation"           : "UPDATE",
            "c_program_id"          : $pid,
            "c_mod_existence"       : false,
            "c_new_existence"       : null,
            "c_mod_program_id"      : false,
            "c_new_program_id"      : null,
            "c_mod_proposal_status" : true,
            "c_new_proposal_status" : "not_submitted"
          }
          """
        )

    for {
      c <- createGeminiCallForProposalsAs(staff, semester = Semester.unsafeFromString("2025A"))
      p <- createProgramWithNonPartnerPi(pi)
      _ <- addProposal(pi, p)
      _ <- addSubmissionPrerequisites(p)
      _ <- setCallId(pi, p, c)
      _ <- addPartnerSplits(pi, p)
      _ <- addCoisAs(pi, p)
      _ <- submit(p)
      _ <- recall(p)
      l <- chronProgramUpdates(p)
    } yield assertEquals(l.drop(4), expected(p))
  }

  test("⨯ edit proposal status (staff can set to ACCEPTED, and pi cannot change it again)") {

    def accept(pid: Program.Id): IO[Unit] =
      expect(
        user = staff,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                programId: "$pid"
                status: ACCEPTED,
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          json"""
            {
              "setProposalStatus" : {
                "program": {
                  "id" : $pid,
                  "proposalStatus": "ACCEPTED"
                 }
              }
            }
          """.asRight
      )

    def recall(pid: Program.Id): IO[Unit] =
      expect(
        user = pi,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                status: NOT_SUBMITTED
                programId: "$pid"
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          List(error.notAuthorizedOld(pid, pi, ProposalStatus.Accepted).message).asLeft
      )

    for {
      c <- createGeminiCallForProposalsAs(staff, semester = Semester.unsafeFromString("2025A"))
      p <- createProgramWithNonPartnerPi(pi)
      _ <- addProposal(pi, p)
      _ <- addSubmissionPrerequisites(p)
      _ <- addPartnerSplits(pi, p)
      _ <- addCoisAs(pi, p)
      _ <- setCallId(pi, p, c)
      _ <- accept(p)
      _ <- recall(p)
    } yield ()
  }

  test("⨯ user cannot set status of another user's proposal") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi2,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                programId: "$pid"
                status: SUBMITTED
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(OdbError.InvalidProgram(pid).message))
      )
    }
  }

  test("⨯ attempt to set proposal status in non-existent program") {
    val badPid = Program.Id.fromLong(Long.MaxValue).get
    expect(
      user = pi,
      query = s"""
        mutation {
          setProposalStatus(
            input: {
              programId: "$badPid"
              status: SUBMITTED
            }
          ) {
            program {
              id
              proposalStatus
            }
          }
        }
      """,
      expected =
        Left(List(OdbError.InvalidProgram(badPid).message))
    )
  }

  test("ensure that configuration requests are created when the proposal is submitted, but not for inactive observations or calibrations") {
    for
      cid <- createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addSubmissionPrerequisites(pid)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- computeItcResultAs(pi, oid)
      ina <- createObservationAs(pi, pid) // inactive, should be ignored
      _   <- setObservationWorkflowState(pi, ina, ObservationWorkflowState.Inactive)
      cal <- createObservationAs(pi, pid, tid) // calibration, should be ignored
      _   <- setObservationCalibrationRole(List(cal), CalibrationRole.Photometric)
      _   <-
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program {
                  configurationRequests {
                    matches {
                      status
                    }
                  }
                }
              }
            }
          """,
          expected =
            json"""
              {
                "setProposalStatus": {
                  "program": {
                    "configurationRequests" : {
                      "matches" : [
                        {
                          "status" : "REQUESTED"
                        }
                      ]
                    }
                  }
                }
              }
            """.asRight
        )
    yield ()

  }

  test("ensure that configuration requests are deleted when the proposal is withdrawn") {
    for
      cid <- createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addSubmissionPrerequisites(pid)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- computeItcResultAs(pi, oid)
      _   <-
        query(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program {
                  id
                }
              }
            }
          """
        )
      _ <- expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: NOT_SUBMITTED
                }
              ) {
                program {
                  configurationRequests {
                    matches {
                      status
                    }
                  }
                }
              }
            }
          """,
          expected =
            json"""
              {
                "setProposalStatus": {
                  "program": {
                    "configurationRequests" : {
                      "matches" : []
                    }
                  }
                }
              }
            """.asRight
        )
    yield ()

  }

  test("✓ A partner of 'HasNonPartner' counts as a US partner for validation"):
    createGeminiCallForProposalsAs(
      staff,
      GeminiCallForProposalsType.RegularSemester,
      partners = List((Partner.US, none), (Partner.CA, none))
    ).flatMap: cid =>
      createProgramWithCaPi(pi).flatMap: pid =>
        addProposal(pi, pid, cid.some) *>
        addSubmissionPrerequisites(pid) *>
        addPartnerSplits(pi, pid) *>
        addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasGeminiPartner(Partner.CA))
          .flatMap(inviteProgramUserDirectly(pi, pid, _)) *>
        addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasNonPartner)
          .flatMap(inviteProgramUserDirectly(pi, pid, _)) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program {
                  id
                  proposalStatus
                }
              }
            }
          """,
          expected = json"""
            {
              "setProposalStatus" : {
                "program": {
                  "id" : $pid,
                  "proposalStatus": "SUBMITTED"
                 }
              }
            }
          """.asRight
        )

  test("Cannot submit past deadline: PI HasNonPartner with US deadline override"):
    createGeminiCallForProposalsAs(
      staff,
      GeminiCallForProposalsType.RegularSemester,
      deadline = yesterday.some,
      partners = List((Partner.US, none), (Partner.CA, none))
    ).flatMap: cid =>
      createProgramWithNonPartnerPi(pi).flatMap: pid =>
        addProposal(pi, pid, cid.some) *>
        addSubmissionPrerequisites(pid) *>
        addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100))) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program {
                  id
                  proposalStatus
                }
              }
            }
          """,
          expected =
            List(error.submissionError(ProposalSubmissionError.PastDeadline, pid).message).asLeft
        )

  test("Cannot submit past deadline: PI HasNonPartner with default US deadline"):
    createGeminiCallForProposalsAs(
      staff,
      GeminiCallForProposalsType.RegularSemester,
      partners = List((Partner.US, yesterday.some), (Partner.CA, none))
    ).flatMap: cid =>
      createProgramWithNonPartnerPi(pi).flatMap: pid =>
        addProposal(pi, pid, cid.some) *>
        addSubmissionPrerequisites(pid) *>
        addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100))) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program {
                  id
                  proposalStatus
                }
              }
            }
          """,
          expected =
            List(error.submissionError(ProposalSubmissionError.PastDeadline, pid).message).asLeft
        )

  test("Cannot submit past deadline: PI HasGeminiPartner with default deadline"):
    createGeminiCallForProposalsAs(
      staff,
      GeminiCallForProposalsType.RegularSemester,
      deadline = yesterday.some,
      partners = List((Partner.US, none), (Partner.CA, none))
    ).flatMap: cid =>
      createProgramWithCaPi(pi).flatMap: pid =>
        addProposal(pi, pid, cid.some) *>
        addSubmissionPrerequisites(pid) *>
        addPartnerSplits(pi, pid, partnerSplits = List((Partner.CA, 100))) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program {
                  id
                  proposalStatus
                }
              }
            }
          """,
          expected =
            List(error.submissionError(ProposalSubmissionError.PastDeadline, pid).message).asLeft
        )

  test("Cannot submit past deadline: PI HasGeminiPartner with deadline override"):
    createGeminiCallForProposalsAs(
      staff,
      GeminiCallForProposalsType.RegularSemester,
      partners = List((Partner.US, none), (Partner.CA, yesterday.some))
    ).flatMap: cid =>
      createProgramWithCaPi(pi).flatMap: pid =>
        addProposal(pi, pid, cid.some) *>
        addSubmissionPrerequisites(pid) *>
        addPartnerSplits(pi, pid, partnerSplits = List((Partner.CA, 100))) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program {
                  id
                  proposalStatus
                }
              }
            }
          """,
          expected =
            List(error.submissionError(ProposalSubmissionError.PastDeadline, pid).message).asLeft
        )

  test("Cannot submit without a PI email address"):
    for
      cid <- createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramAs(pi)
      mid <- piProgramUserIdAs(pi, pid)
      _   <- updateProgramUserAs(pi, mid, PartnerLink.HasNonPartner, email = none)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addSubmissionPrerequisites(pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100)))
      _   <-
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program {
                  proposalStatus
                }
              }
            }
          """,
          expected =
            List(error.submissionError(ProposalSubmissionError.MissingPiEmail, pid).message).asLeft
        )
    yield ()

  test("Cannot submit with an invalid PI email address"):
    for
      cid <- createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramAs(pi)
      mid <- piProgramUserIdAs(pi, pid)
      em   = NonEmptyString.unsafeFrom("invalid")
      _   <- updateProgramUserAs(pi, mid, PartnerLink.HasNonPartner, email = em.some)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addSubmissionPrerequisites(pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100)))
      _   <-
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program {
                  proposalStatus
                }
              }
            }
          """,
          expected =
            List(error.submissionError(ProposalSubmissionError.InvalidPiEmail, pid).message).asLeft
        )
    yield ()

  test("⨯ queue submission requires band 3 flag"):
    for
      cid <- createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some, "queue: { explicitTooActivationCeiling: NONE, minPercentTime: 0, considerForBand3: UNSET }".some)
      _   <- addSubmissionPrerequisites(pid)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      _   <- expect(
              user = pi,
              query = s"""
                mutation {
                  setProposalStatus(
                    input: {
                      programId: "$pid"
                      status: SUBMITTED
                    }
                  ) {
                    program { proposal { reference { label } } }
                  }
                }
              """,
              expected = List(
                error.submissionError(ProposalSubmissionError.MissingBand3Consideration, pid).message
              ).asLeft
            )
    yield ()

  test("✓ classical submission does not take a band 3 flag"):
    for
      cid <- createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some, "classical: { minPercentTime: 0 }".some)
      _   <- addSubmissionPrerequisites(pid)
      _   <- addPartnerSplits(pi, pid, "classical")
      _   <- addCoisAs(pi, pid)
      _   <- expect(
              user = pi,
              query = s"""
                mutation {
                  setProposalStatus(
                    input: {
                      programId: "$pid"
                      status: SUBMITTED
                    }
                  ) {
                    program { proposalStatus }
                  }
                }
              """,
              expected = json"""
                {
                  "setProposalStatus": {
                    "program": {
                      "proposalStatus": "SUBMITTED"
                    }
                  }
                }
              """.asRight
            )
    yield ()


  // ---------------------------------------------------------------------------
  // Rules shared with the proposal editor.  Each starts from a submittable
  // proposal and takes away the one thing the rule is about.
  // ---------------------------------------------------------------------------

  private def submitExpecting(pid: Program.Id, errors: ProposalSubmissionError*): IO[Unit] =
    expect(
      user  = pi,
      query = s"""
        mutation {
          setProposalStatus(input: { programId: "$pid", status: SUBMITTED }) {
            program { proposalStatus }
          }
        }
      """,
      expected = errors.toList.map(error.submissionError(_, pid).message).asLeft
    )

  private def submitExpectingSuccess(pid: Program.Id): IO[Unit] =
    expect(
      user  = pi,
      query = s"""
        mutation {
          setProposalStatus(input: { programId: "$pid", status: SUBMITTED }) {
            program { proposalStatus }
          }
        }
      """,
      expected = json"""
        { "setProposalStatus": { "program": { "proposalStatus": "SUBMITTED" } } }
      """.asRight
    )

  /**
   * A proposal that is submittable except for the pieces switched off here.
   */
  private def proposalMissing(
    cid:             CallForProposals.Id,
    title:           Boolean = true,
    abstrakt:        Boolean = true,
    scienceAttach:   Boolean = true,
    teamAttach:      Boolean = true,
    observation:     Boolean = true,
    proposalType:    String  = "queue: { considerForBand3: DO_NOT_CONSIDER }"
  ): IO[Program.Id] =
    for
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some, proposalType.some)
      _   <- addProposalPrerequisitesAs(pi, pid, title, abstrakt, scienceAttach, teamAttach)
      _   <- addDefinedObservationAs(pi, pid).whenA(observation)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
    yield pid

  test("⨯ missing title") {
    createGeminiCallForProposalsAs(staff).flatMap: cid =>
      proposalMissing(cid, title = false).flatMap(submitExpecting(_, MissingTitle))
  }

  test("⨯ missing abstract") {
    createGeminiCallForProposalsAs(staff).flatMap: cid =>
      proposalMissing(cid, abstrakt = false).flatMap(submitExpecting(_, MissingAbstract))
  }

  test("⨯ missing science attachment") {
    createGeminiCallForProposalsAs(staff).flatMap: cid =>
      proposalMissing(cid, scienceAttach = false).flatMap(submitExpecting(_, MissingScienceAttachment))
  }

  test("⨯ missing team attachment") {
    createGeminiCallForProposalsAs(staff).flatMap: cid =>
      proposalMissing(cid, teamAttach = false).flatMap(submitExpecting(_, MissingTeamAttachment))
  }

  test("⨯ no defined observation") {
    createGeminiCallForProposalsAs(staff).flatMap: cid =>
      proposalMissing(cid, observation = false).flatMap(submitExpecting(_, NoDefinedObservations))
  }

  // Calibrations are always Defined, so one on its own must not stand in for the
  // science observation the proposal is required to have.
  test("⨯ a calibration alone is not a defined observation") {
    for
      cid <- createGeminiCallForProposalsAs(staff)
      pid <- proposalMissing(cid, observation = false)
      tid <- createTargetWithProfileAs(pi, pid)
      cal <- createObservationAs(pi, pid, tid)
      _   <- setObservationCalibrationRole(List(cal), CalibrationRole.Photometric)
      _   <- submitExpecting(pid, NoDefinedObservations)
    yield ()
  }

  test("⨯ missing category") {
    for
      cid <- createGeminiCallForProposalsAs(staff)
      pid <- proposalMissing(cid)
      _   <- query(
               pi,
               s"""
                 mutation {
                   updateProposal(input: { programId: "$pid", SET: { category: null } }) {
                     proposal { category }
                   }
                 }
               """
             )
      _   <- submitExpecting(pid, MissingCategory)
    yield ()
  }

  test("⨯ an investigator with no affiliation") {
    for
      cid <- createGeminiCallForProposalsAs(staff)
      pid <- proposalMissing(cid)
      _   <- addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasUnspecifiedPartner)
               .flatMap(inviteProgramUserDirectly(pi, pid, _))
      _   <- submitExpecting(pid, UnspecifiedInvestigatorPartner)
    yield ()
  }

  test("⨯ an investigator who was never invited") {
    for
      cid <- createGeminiCallForProposalsAs(staff)
      pid <- proposalMissing(cid)
      _   <- addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasGeminiPartner(Partner.US))
      _   <- submitExpecting(pid, UninvitedInvestigator)
    yield ()
  }

  test("⨯ PI partner the call does not offer") {
    for
      cid <- createGeminiCallForProposalsAs(staff, partners = List((Partner.US, none)))
      pid <- createProgramWithCaPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addSubmissionPrerequisites(pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100)))
      _   <- addCoisAs(pi, pid, List(Partner.US))
      // The call has no CA deadline either, the PI's partner being unoffered.
      _   <- submitExpecting(pid, PiPartnerNotInCall, MissingDeadline)
    yield ()
  }

  // A call offers non-partner PIs only when US is among its partners.
  test("⨯ non-partner PI the call does not allow") {
    for
      cid <- createGeminiCallForProposalsAs(staff, partners = List((Partner.CA, none)))
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addSubmissionPrerequisites(pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.CA, 100)))
      _   <- addCoisAs(pi, pid, List(Partner.CA))
      _   <- submitExpecting(pid, NonPartnerPiNotAllowed, MissingDeadline)
    yield ()
  }

  test("⨯ UH time without a UH PI") {
    for
      cid <- createGeminiCallForProposalsAs(staff, partners = List((Partner.US, none), (Partner.UH, none)))
      pid <- createProgramWithUsPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addSubmissionPrerequisites(pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.UH, 100)))
      _   <- addCoisAs(pi, pid, List(Partner.UH))
      _   <- submitExpecting(pid, UhTimeWithoutUhPi)
    yield ()
  }

  test("⨯ fast turnaround without a mentor for a non-PhD reviewer") {
    for
      cid <- createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.FastTurnaround)
      pid <- createProgramWithUsPi(pi)
      _   <- setPiEducationalStatusAs(pi, pid, EducationalStatus.GradStudent)
      _   <- addProposal(pi, pid, cid.some, "fastTurnaround: {}".some)
      // Fast Turnaround proposals are reviewed without a team attachment.
      _   <- addProposalPrerequisitesAs(pi, pid, includeTeamAttachment = false)
      _   <- addDefinedObservationAs(pi, pid)
      _   <- addCoisAs(pi, pid, List(Partner.US))
      _   <- submitExpecting(pid, MissingFtMentor)
    yield ()
  }

  // ---------------------------------------------------------------------------
  // The proposal's exchange partner must agree with the PI's affiliation.  Only
  // the API can put the two out of step; the proposal editor derives one from
  // the other.
  // ---------------------------------------------------------------------------

  private def exchangeProposal(piLink: PartnerLink, proposalType: String): IO[Program.Id] =
    for
      cid <- createCallWithKeckExchangeAs(
               Semester.unsafeFromString("2027A"),
               LocalDate.parse("2027-02-01"),
               LocalDate.parse("2027-07-31")
             )
      pid <- createProgramWithPiAffiliation(pi, piLink)
      _   <- addProposal(pi, pid, cid.some, proposalType.some)
      _   <- addSubmissionPrerequisites(pid)
      _   <- addCoisAs(pi, pid)
    yield pid

  test("⨯ exchange partner with a Gemini partner PI") {
    exchangeProposal(
      PartnerLink.HasGeminiPartner(Partner.US),
      "classical: { exchangePartner: KECK }"
    ).flatMap(submitExpecting(_, ExchangePartnerPiMismatch))
  }

  test("⨯ exchange partner with a non-partner PI") {
    exchangeProposal(
      PartnerLink.HasNonPartner,
      "classical: { exchangePartner: KECK }"
    ).flatMap(submitExpecting(_, ExchangePartnerPiMismatch))
  }

  test("⨯ exchange PI whose proposal names no exchange partner") {
    for
      pid <- exchangeProposal(
               PartnerLink.HasExchangePartner(ExchangePartner.Keck),
               "classical: { partnerSplits: [ { partner: US, percent: 100 } ] }"
             )
      // With no exchange partner on the proposal there is no community deadline
      // for the call to fall back on.
      _   <- submitExpecting(pid, ExchangePartnerPiMismatch, MissingDeadline)
    yield ()
  }

  // A co-investigator's exchange affiliation says nothing about the time
  // request, so it leaves the partner splits requirement in place.
  test("⨯ a COI exchange partner does not waive the partner splits") {
    for
      cid <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addSubmissionPrerequisites(pid)
      _   <- addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasExchangePartner(ExchangePartner.Keck))
               .flatMap(inviteProgramUserDirectly(pi, pid, _))
      _   <- submitExpecting(pid, InvalidPartnerSplits)
    yield ()
  }


  test("every rule is accounted for") {
    // Every rule in the shared enum is exercised by a test above, save the one
    // noted below.  Adding a rule in lucuma-core fails this until it is covered
    // here too.
    val elsewhere: Set[ProposalSubmissionError] =
      // Rejected at edit time rather than submission; see updateProposal's
      // "cannot have both partner splits and an exchange partner".
      Set(BothTimeRequests)

    val exercised: Set[ProposalSubmissionError] =
      Set(
        MissingTitle, MissingAbstract, MissingCategory, MissingCfp, MissingProposalType,
        MissingSemester, PiPartnerNotInCall, NonPartnerPiNotAllowed, ExchangePartnerNotInCall,
        ExchangePartnerPiMismatch, MissingBand3Consideration, UnspecifiedInvestigatorPartner,
        InvalidPartnerSplits, MissingPiEmail, InvalidPiEmail, UninvitedInvestigator,
        UhTimeWithoutUhPi, UnmatchedPartnerTime, MissingFtMentor, MissingScienceAttachment,
        MissingTeamAttachment, NoDefinedObservations, UndefinedObservations,
        MissingDeadline, PastDeadline
      )

    IO(assertEquals(Enumerated[ProposalSubmissionError].all.toSet -- exercised -- elsewhere, Set.empty))
  }

  // Discards the program's cached ITC results, as an ITC version bump does for
  // every non-frozen result.
  private def purgeItcResults(pid: Program.Id): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"DELETE FROM t_itc_result WHERE c_program_id = $program_id".command
      )(pid).void

  // The workflow computation behind proposal submission reads cached ITC
  // results and never calls the ITC, so an absent result is indistinguishable
  // from a failed one.  A purged cache must not make every observation look
  // undefined.
  test("✓ valid submission with a purged ITC cache") {
    createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramWithNonPartnerPi(pi).flatMap { pid =>
        addProposal(pi, pid, cid.some) *>
        addSubmissionPrerequisites(pid) *>
        addPartnerSplits(pi, pid) *>
        addCoisAs(pi, pid) *>
        purgeItcResults(pid) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program { proposalStatus }
              }
            }
          """,
          expected =
            json"""
              {
                "setProposalStatus": {
                  "program": { "proposalStatus": "SUBMITTED" }
                }
              }
            """.asRight
        )
      }
    }
  }

}
