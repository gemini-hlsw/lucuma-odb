// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.syntax.option.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.Partner
import lucuma.odb.Config
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.refined.*
import org.http4s.implicits.*

// The notification email for a proposal with a real observation, which is the only
// way to exercise a non-empty time request and instrument list.
class setProposalStatusEmailTime extends ExecutionTestSupportForGmos {

  override val httpRequestHandler = invitationEmailRequestHandler

  private val proposalAddress: EmailAddress =
    EmailAddress.unsafeFrom("us@gemini.edu")

  override def emailConfig: Config.Email =
    Config.Email(
      apiKey            = "apiKey".refined,
      domain            = "gpp.com".refined,
      webhookSigningKey = "webhookKey".refined,
      invitationFrom    = EmailAddress.unsafeFrom("explore@gpp.com"),
      exploreUrl        = uri"https://explore.gemini.edu/",
      proposalEmails    = Config.ProposalEmails.uniform(proposalAddress)
    )

  test("✓ the request and instruments come from the program's observations") {
    for {
      cid <- createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramWithNonPartnerPi(pi, "Timed Proposal")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- runObscalcUpdate(pid, oid)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100)))
      _   <- addCoisAs(pi, pid, List(Partner.US))
      _   <- submitProposal(pi, pid)
      ms  <- getEmailMessages(pid, proposalAddress)
    } yield {
      val (_, text, html) = ms.head
      assertEquals(ms.size, 1)
      assertEquals(lineOf(text, "Instruments"), "Instruments: GMOS North")
      assertEquals(lineOf(html.get, "Instruments"), "Instruments: GMOS North<br/>")

      // The figure follows from the sequence estimate, so only its shape is checked.
      val request = lineOf(text, "Request")
      assert(
        request.matches("""Request: \d+\.\d{2}( - \d+\.\d{2})? hours"""),
        s"Unexpected request line: $request"
      )
    }
  }

  test("✓ an explicit request is what the notification reports") {
    for {
      cid <- createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramWithNonPartnerPi(pi, "Explicitly Timed Proposal")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- runObscalcUpdate(pid, oid)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100)))
      _   <- addCoisAs(pi, pid, List(Partner.US))
      _   <- query(
               pi,
               s"""
                 mutation {
                   updateProposal(
                     input: {
                       programId: "$pid"
                       SET: { explicitTimeRequest: { hours: 42 } }
                     }
                   ) {
                     proposal { explicitTimeRequest { hours } }
                   }
                 }
               """
             )
      _   <- submitProposal(pi, pid)
      ms  <- getEmailMessages(pid, proposalAddress)
    } yield {
      val (_, text, html) = ms.head
      assertEquals(ms.size, 1)
      // The observation's own estimate is nowhere near 42 hours, so this shows
      // the explicit request displacing the derived sum rather than adding to it.
      assertEquals(lineOf(text, "Request"), "Request: 42.00 hours")
      assertEquals(lineOf(html.get, "Request"), "Request: 42.00 hours<br/>")
    }
  }

  private def lineOf(message: String, label: String): String =
    message.linesIterator.find(_.startsWith(s"$label:")).getOrElse(s"<no $label line in: $message>")

}
