// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.effect.IO
import cats.syntax.option.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.CallForProposals
import lucuma.core.model.Program
import lucuma.core.model.ProgramUser
import lucuma.core.model.ProposalReference
import lucuma.core.model.UserProfile
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
      // The call has to invite the Keck community before it can be asked for time
      // on its behalf.
      cid <- createGeminiCallForProposalsAs(
               staff,
               GeminiCallForProposalsType.RegularSemester,
               otherGemini = "exchangePartners: [{ exchangePartner: KECK }]".some
             )
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

  test("✓ the notification message names the proposal, the investigators and the request") {
    val expectedText = (ref: ProposalReference) =>
      s"""|A new Queue proposal has been received:
          |Id: ${ref.label}
          |URL: https://explore.gemini.edu/${ref.label}
          |Title: Ann & Bob's Big Adventure
          |PI: Petra Ito (Gemini Observatory)
          |CoIs: Ann Coi, Zoe CoiRO
          |Request: Not available
          |Instruments: None
          |Abstract: A study of <interesting> things.""".stripMargin

    val expectedHtml = (ref: ProposalReference) =>
      s"""|A new Queue proposal has been received:<br/>
          |Id: ${ref.label}<br/>
          |URL: <a href="https://explore.gemini.edu/${ref.label}">https://explore.gemini.edu/${ref.label}</a><br/>
          |Title: Ann &amp; Bob's Big Adventure<br/>
          |PI: Petra Ito (Gemini Observatory)<br/>
          |CoIs: Ann Coi, Zoe CoiRO<br/>
          |Request: Not available<br/>
          |Instruments: None<br/>
          |Abstract: A study of &lt;interesting&gt; things.""".stripMargin

    for {
      cid <- geminiCall(GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramWithNonPartnerPi(pi, "Ann & Bob's Big Adventure")
      _   <- setProgramDescription(pid, "A study of <interesting> things.")
      pu  <- piProgramUserIdAs(pi, pid)
      _   <- setCreditName(pu, "Petra Ito")
      _   <- addProgramUserAs(pi, pid, ProgramUserRole.Coi, preferred = creditName("Ann Coi"))
      _   <- addProgramUserAs(pi, pid, ProgramUserRole.CoiRO, preferred = creditName("Zoe CoiRO"))
      _   <- addProposal(pi, pid, cid.some)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100)))
      ref <- submitProposal(pi, pid)
      ms  <- getEmailMessages(pid, address("us"))
    } yield assertEquals(
      ms,
      List((s"New Queue proposal received: ${ref.label}", expectedText(ref), expectedHtml(ref).some))
    )
  }

  private def creditName(name: String): UserProfile =
    UserProfile(givenName = none, familyName = none, creditName = name.some, email = none)

  private def setCreditName(puid: ProgramUser.Id, name: String): IO[Unit] =
    query(
      user  = pi,
      query = s"""
        mutation {
          updateProgramUsers(input: {
            WHERE: { id: { EQ: "$puid" } }
            SET: { preferredProfile: { creditName: "$name" } }
          }) {
            programUsers { id }
          }
        }
      """
    ).void

  private def setProgramDescription(pid: Program.Id, description: String): IO[Unit] =
    query(
      user  = pi,
      query = s"""
        mutation {
          updatePrograms(input: {
            WHERE: { id: { EQ: "$pid" } }
            SET: { description: "$description" }
          }) {
            programs { id }
          }
        }
      """
    ).void

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
