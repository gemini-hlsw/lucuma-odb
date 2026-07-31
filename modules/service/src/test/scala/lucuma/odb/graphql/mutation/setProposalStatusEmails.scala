// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.effect.IO
import cats.syntax.option.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.Partner
import lucuma.core.model.CallForProposals
import lucuma.core.model.Program
import lucuma.odb.Config
import lucuma.refined.*
import org.http4s.implicits.*

// Tests the notification emails that are sent, in addition to the PI confirmation
// email, when a proposal is submitted.
class setProposalStatusEmails extends OdbSuite {

  val pi    = TestUsers.Standard.pi(1, 101)
  val staff = TestUsers.Standard.staff(4, 104)

  val validUsers = List(pi, staff)

  override val httpRequestHandler = invitationEmailRequestHandler

  private def address(name: String): EmailAddress =
    EmailAddress.unsafeFrom(s"$name@gemini.edu")

  // AR and BR share an address so that duplicate recipients can be tested.
  private val ngoShared = address("ngo-shared")

  override def emailConfig: Config.Email =
    Config.Email(
      apiKey            = "apiKey".refined,
      domain            = "gpp.com".refined,
      webhookSigningKey = "webhookKey".refined,
      invitationFrom    = EmailAddress.unsafeFrom("explore@gpp.com"),
      exploreUrl        = uri"https://explore.gemini.edu/",
      proposalEmails    = Config.ProposalEmails(
        demoScience        = address("demo-science"),
        directorsTime      = address("directors-time"),
        fastTurnaround     = address("fast-turnaround"),
        largeProgram       = address("large-program"),
        poorWeather        = address("poor-weather"),
        systemVerification = address("system-verification"),
        subaru             = address("subaru"),
        keck               = address("keck"),
        ar                 = ngoShared,
        br                 = ngoShared,
        ca                 = address("ca"),
        cl                 = address("cl"),
        kr                 = address("kr"),
        uh                 = address("uh"),
        us                 = address("us")
      )
    )

  private val piAddress: EmailAddress =
    EmailAddress.unsafeFrom(defaultPiEmail.value)

  private def assertRecipients(pid: Program.Id, expected: List[EmailAddress]): IO[Unit] =
    getEmailRecipients(pid).map(assertEquals(_, expected.sortBy(_.value.value)))

  private def geminiCall(callType: GeminiCallForProposalsType): IO[CallForProposals.Id] =
    createGeminiCallForProposalsAs(staff, callType)

  test("✓ fast turnaround notifies the fast turnaround address") {
    for {
      cid <- geminiCall(GeminiCallForProposalsType.FastTurnaround)
      pid <- createProgramWithUsPi(pi)
      _   <- addProposal(pi, pid, cid.some, "fastTurnaround: {}".some)
      _   <- addCoisAs(pi, pid, List(Partner.US))
      _   <- submitProposal(pi, pid)
      _   <- assertRecipients(pid, List(piAddress, address("fast-turnaround")))
    } yield ()
  }

  test("✓ director's time notifies the director's time address") {
    for {
      cid <- geminiCall(GeminiCallForProposalsType.DirectorsTime)
      pid <- createProgramWithUsPi(pi)
      _   <- addProposal(pi, pid, cid.some, "directorsTime: {}".some)
      _   <- addCoisAs(pi, pid, List(Partner.US))
      _   <- submitProposal(pi, pid)
      _   <- assertRecipients(pid, List(piAddress, address("directors-time")))
    } yield ()
  }

  test("✓ regular semester notifies each partner in the splits") {
    for {
      cid <- geminiCall(GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 70), (Partner.CA, 30)))
      _   <- addCoisAs(pi, pid, List(Partner.US, Partner.CA))
      _   <- submitProposal(pi, pid)
      _   <- assertRecipients(pid, List(piAddress, address("us"), address("ca")))
    } yield ()
  }

  test("✓ a partner with a zero percent split is not notified") {
    for {
      cid <- geminiCall(GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100), (Partner.CA, 0)))
      _   <- addCoisAs(pi, pid, List(Partner.US, Partner.CA))
      _   <- submitProposal(pi, pid)
      _   <- assertRecipients(pid, List(piAddress, address("us")))
    } yield ()
  }

  test("✓ partners sharing an address are notified once") {
    for {
      cid <- geminiCall(GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.AR, 50), (Partner.BR, 50)))
      _   <- addCoisAs(pi, pid, List(Partner.AR, Partner.BR))
      _   <- submitProposal(pi, pid)
      _   <- assertRecipients(pid, List(piAddress, ngoShared))
    } yield ()
  }

  test("✓ an exchange partner request notifies the exchange partner") {
    for {
      cid <- geminiCall(GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some, "classical: { exchangePartner: KECK }".some)
      _   <- addCoisAs(pi, pid)
      _   <- submitProposal(pi, pid)
      _   <- assertRecipients(pid, List(piAddress, address("keck")))
    } yield ()
  }

  test("✓ a Subaru proposal notifies no one") {
    for {
      cid <- createSubaruCallForProposalsAs(staff)
      // A US PI, since a Subaru call has no deadline for a non-partner.
      pid <- createProgramWithUsPi(pi)
      _   <- createSubaruProposal(pid, cid)
      _   <- submitProposal(pi, pid)
      _   <- assertRecipients(pid, List(piAddress))
    } yield ()
  }

  test("✓ no emails are sent unless the proposal is submitted") {
    for {
      cid <- geminiCall(GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100)))
      _   <- addCoisAs(pi, pid, List(Partner.US))
      _   <- assertRecipients(pid, Nil)
      _   <- submitProposal(pi, pid)
      _   <- unsubmitProposal(pi, pid)
      _   <- assertRecipients(pid, List(piAddress, address("us")))
    } yield ()
  }

  // An external (Subaru) proposal apportions its time across Gemini partners, so it
  // carries its splits directly.  There is no `addProposal` variant for one.
  private def createSubaruProposal(
    pid: Program.Id,
    cid: CallForProposals.Id
  ): IO[Unit] =
    query(
      user  = pi,
      query = s"""
        mutation {
          createProposal(
            input: {
              programId: "$pid"
              SET: {
                category: GALACTIC_OTHER
                callId: "$cid"
                subaru: { partnerSplits: [{ partner: US, percent: 100 }] }
              }
            }
          ) {
            proposal { category }
          }
        }
      """
    ).void

}
