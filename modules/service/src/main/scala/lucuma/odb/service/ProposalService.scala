// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.ConsiderForBand3
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.Observatory
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ProposalStatus
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.SubaruCallForProposalsType
import lucuma.core.model.Access
import lucuma.core.model.CallForProposals
import lucuma.core.model.IntPercent
import lucuma.core.model.Program
import lucuma.core.model.ProposalReference
import lucuma.core.model.Semester
import lucuma.core.model.User
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.core.util.CalculatedValue
import lucuma.core.util.CalculationState
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.data.*
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.CreateProposalInput
import lucuma.odb.graphql.input.DeleteProposalInput
import lucuma.odb.graphql.input.GeminiProposalTypeInput
import lucuma.odb.graphql.input.ProposalPropertiesInput
import lucuma.odb.graphql.input.SetProposalStatusInput
import lucuma.odb.graphql.input.UpdateProposalInput
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.syntax.scienceSubtype.*
import lucuma.odb.util.Codecs.*
import org.http4s.Uri
import skunk.*
import skunk.codec.all.*
import skunk.data.Completion.Delete
import skunk.data.Completion.Update
import skunk.syntax.all.*

import java.time.format.DateTimeFormatter

import Services.Syntax.*

private[service] trait ProposalService[F[_]] {

  /**
   * Create a proposal associated with the program specified in `input`.
   */
  def createProposal(
    input: CreateProposalInput
  )(using Transaction[F], Services.PiAccess): F[Result[Program.Id]]

  /**
   * Update a proposal associated with the program specified in the `input`.
   */
  def updateProposal(
    input: UpdateProposalInput
  )(using Transaction[F], Services.PiAccess): F[Result[Program.Id]]

  /**
   * The observing time requested by the proposal in the given program: the
   * explicit request if one has been made, and otherwise the sum of the time
   * estimates of the program's observations.
   */
  def timeRequest(
    pid: Program.Id
  )(using Transaction[F]): F[Option[CalculatedValue[CategorizedTimeRange]]]

  /**
   * Checks whether a proposal is defined for the given program.
   */
  def hasProposal(
    pid: Program.Id
  )(using Transaction[F]): F[Boolean]

  /**
   * Deletes a proposal associated with the given pid, if any.
   * @return `true`` if a proposal is deleted, `false` otherwise
   */
  def deleteProposal(
    input: DeleteProposalInput
  )(using Transaction[F], Services.StaffAccess): F[Result[Boolean]]

  /**
   * Set the proposal status associated with the program specified in the `input`.
   */
  def setProposalStatus(
    input: SetProposalStatusInput,
    commitHash: CommitHash,
    itcClient: ItcClient[F],
    ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
  )(using NoTransaction[F], Services.PiAccess): F[Result[Program.Id]]

}

object ProposalService {

  import CallForProposalsService.CfpProperties

  object error {
    extension (s: String)
      def invalidArg: OdbError           = OdbError.InvalidArgument(s.some)
      def noAuth(uid: User.Id): OdbError = OdbError.NotAuthorized(uid, s.some)

    def cfpNotFound(cid: CallForProposals.Id): OdbError =
      s"The specified Call for Proposals $cid was not found.".invalidArg

    def creationFailed(pid: Program.Id): OdbError =
      s"Proposal creation failed because program $pid already has a proposal.".invalidArg

    def updateFailed(pid: Program.Id): OdbError =
      s"Proposal update failed because program $pid does not have a proposal.".invalidArg

    def invalidProposalStatus(ps: Tag): OdbError =
      s"Invalid proposal status: ${ps.value}".invalidArg

    def missingCfP(pid: Program.Id): OdbError =
      s"A Call for Proposals must be selected for $pid before submitting a proposal.".invalidArg

    def missingSemester(pid: Program.Id): OdbError =
      s"Submitted proposal $pid must be associated with a semester.".invalidArg

    def missingScienceSubtype(pid: Program.Id): OdbError =
      s"Submitted proposal $pid must have a science subtype.".invalidArg

    def missingOrInvalidSplits(pid: Program.Id, subtype: ScienceSubtype): OdbError =
      s"Submitted proposal $pid of type ${subtype.title} must specify partner time percentages which sum to 100%.".invalidArg

    def missingOrInvalidSplitsExternal(pid: Program.Id): OdbError =
      s"Submitted external proposal $pid must specify partner time percentages which sum to 100%.".invalidArg

    def bothTimeRequests(pid: Program.Id): OdbError =
      s"Proposal $pid may not have both an exchange partner and partner splits.".invalidArg

    def unofferedExchangePartner(pid: Program.Id, xp: ExchangePartner): OdbError =
      s"Program $pid requests time on behalf of ${xp.tag.toUpperCase}, but the Call for Proposals does not offer that exchange partner.".invalidArg

    def missingPartners(pid: Program.Id, partners: Set[Partner] = Set.empty): OdbError =
      partners.toList.map(_.abbreviation).sorted match
        case Nil     =>
          s"Program $pid requests time from partners not represented by any investigator.".invalidArg
        case List(p) =>
          s"Program $pid requests time from $p, but there is no matching investigator with this partner.".invalidArg
        case ps      =>
          s"Program $pid requests time from ${ps.init.mkString(", ")} and ${ps.last}, but there are no matching investigators with these partners.".invalidArg

    // If all the other validations pass, I don't think we should get this...
    def missingDeadline(pid: Program.Id): OdbError =
      s"Could not determine the deadline for the call for proposals for program $pid.".invalidArg

    def pastDeadline(pid: Program.Id): OdbError =
      s"Call for proposals for program $pid has passed its deadline.".invalidArg

    def invalidProgramType(pid: Program.Id, progType: ProgramType): OdbError =
      s"Program $pid is of type $progType. Only Science programs can have proposals.".invalidArg

    def missingProposal(pid: Program.Id): OdbError =
      s"Proposal status in program $pid cannot be changed because it has no proposal.".invalidArg

    def cannotEditSubmittedProposal(pid: Program.Id, user: User): OdbError =
      s"User ${user.id} cannot edit this proposal $pid because it has been submitted.".noAuth(user.id)

    def notAuthorizedNew(pid: Program.Id, user: User, ps: ProposalStatus): OdbError =
      s"User ${user.id} not authorized to set proposal status to ${ps.tag.toUpperCase} in program $pid.".noAuth(user.id)

    def notAuthorizedOld(pid: Program.Id, user: User, ps: ProposalStatus): OdbError =
      s"User ${user.id} not authorized to change proposal status from ${ps.tag.toUpperCase} in program $pid.".noAuth(user.id)

    def undefinedObservations(pid: Program.Id): OdbError =
      s"Submitted proposal $pid contains undefined observations.".invalidArg

    def missingPiEmailAddress(pid: Program.Id): OdbError =
      s"Missing email address for PI in program $pid".invalidArg

    def invalidPiEmailAddress(email: String, pid: Program.Id): OdbError =
      s"Invalid email address \"$email\" for PI in program $pid".invalidArg

    def missingConsiderForBand3(pid: Program.Id): OdbError =
      s"Proposal $pid must specify whether it should be considered for Band 3 before it can be submitted.".invalidArg

  }

  /** Stand-in for a value that the proposal doesn't have. */
  private val Missing: String = "<Missing>"

  /** Stand-in for a list that has no elements. */
  private val Empty: String = "None"

  private def orMissing(s: Option[String]): String =
    s.map(_.trim).filter(_.nonEmpty).getOrElse(Missing)

  /**
   * The observing time requested for a program, in decimal hours.  A single value is
   * given when the minimum and maximum agree.  The calculation state is ignored: a
   * stale estimate is still the best one available.
   */
  private[odb] def timeRequestedText(cv: Option[CalculatedValue[CategorizedTimeRange]]): String =
    def hours(t: TimeSpan): String = f"${t.toHours}%.2f"

    cv.map(_.value).filter(_.max.programTime.toMicroseconds > 0L) match
      case None                                               => "Not available"
      case Some(r) if r.min.programTime === r.max.programTime => s"${hours(r.max.programTime)} hours"
      case Some(r)                                            => s"${hours(r.min.programTime)} - ${hours(r.max.programTime)} hours"

  /**
   * Presents an explicit time request in the shape of the derived one: a
   * settled, degenerate range charged entirely to program time.  Clients then
   * read the explicit and derived cases through a single field.
   */
  private[odb] def explicitTimeRequestRange(ts: TimeSpan): CalculatedValue[CategorizedTimeRange] =
    CalculatedValue(
      CalculationState.Ready,
      CategorizedTimeRange.single(CategorizedTime(ChargeClass.Program -> ts))
    )

  /** Escapes the characters that would otherwise be markup in an html message. */
  private[odb] def escapeHtml(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  /** Construct a `ProposalService` using the specified `Session`. */
  def instantiate[F[_]: Concurrent](emailConfig: Config.Email)(using Services[F]): ProposalService[F] =
    new ProposalService[F] {

      import error.*

      extension (ps: ProposalStatus)
        def userCanChangeStatus: Boolean =
          ps <= ProposalStatus.Submitted ||
          user.role.access >= Access.Ngo

        def userCanEditProposal: Boolean =
          ps < ProposalStatus.Submitted ||
          user.role.access >= Access.Ngo

      def lookupProperties(cid: CallForProposals.Id)(using Transaction[F]): F[Result[CfpProperties]] =
        callForProposalsService
          .selectProperties(cid)
          .map(o => Result.fromOption(o, cfpNotFound(cid).asProblem))

      case class ProposalContext(
        status:            ProposalStatus,
        hasProposal:       Boolean,
        piEmailStr:        Option[NonEmptyString],
        piName:            Option[String],
        title:             Option[NonEmptyString],
        description:       Option[String],
        reference:         Option[ProposalReference],
        semester:          Option[Semester],
        scienceSubtype:    Option[ScienceSubtype],
        splitsSum:         Long,
        availablePartners: Set[Partner],
        requestedPartners: Set[Partner],
        coiNames:          List[String],
        instruments:       List[Instrument],
        proprietary:       NonNegInt,
        currentTime:       Timestamp,
        deadline:          Option[Timestamp],
        cfpTitle:          Option[NonEmptyString],
        cfp:               Option[CfpProperties],
        considerForBand3:  Option[ConsiderForBand3],
        exchangePartner:   Option[ExchangePartner],
        // Whether the call offers the exchange partner named above.  False when
        // there is no exchange partner to begin with.
        exchangeOffered:   Boolean,
        observatory:       Option[Observatory]
      ) {
        // Every stored proposal has an observatory; default to Gemini defensively.
        val obs: Observatory = observatory.getOrElse(Observatory.Gemini)
        val isExternal: Boolean = obs =!= Observatory.Gemini

        // The Subaru proposal type is a property of the linked call, not of the
        // proposal itself.  It is non-empty only for a Subaru proposal (whose
        // call is a Subaru call); the t_program mirror is fed from here.
        val subaruProposalType: Option[SubaruCallForProposalsType] =
          cfp.flatMap(_.subaruProposalType)

        // The Gemini call type, which is empty for an external (Keck/Subaru) proposal.
        val geminiCallType: Option[GeminiCallForProposalsType] =
          cfp.flatMap(_.gemini).map(_.callType)

        val isPastDeadline: Option[Boolean] =
          deadline.map(_ < currentTime)

        // Descriptions of the proposal for the emails sent upon submission.  A missing
        // value is reported rather than left blank, as in the PI submission email.
        val typeText: String =
          scienceSubtype.fold(Missing)(_.title)

        val titleText: String =
          title.fold(Missing)(_.value)

        val cfpTitleText: String =
          cfpTitle.fold(Missing)(_.value)

        val piNameText: String =
          orMissing(piName)

        val abstractText: String =
          orMissing(description)

        // The observatory the PI is associated with, which is Gemini unless the time
        // request is on behalf of an exchange partner community.
        val piObservatory: String =
          exchangePartner.fold("Gemini Observatory"):
            case ExchangePartner.Keck   => "Keck Observatory"
            case ExchangePartner.Subaru => "Subaru Observatory"

        val coiNamesText: String =
          if coiNames.isEmpty then Empty else coiNames.mkString(", ")

        val instrumentsText: String =
          if instruments.isEmpty then Empty else instruments.map(_.longName).sorted.mkString(", ")

        val piEmailAddress: Option[EmailAddress] =
          piEmailStr.flatMap(nes =>
            EmailAddress.from(nes.value).toOption
          )

        def validatePiEmailAddress(pid: Program.Id): Result[Unit] =
          piEmailStr.fold(missingPiEmailAddress(pid).asFailure)(emailStr =>
            piEmailAddress.fold(invalidPiEmailAddress(emailStr.value, pid).asFailure)(_ => Result.unit)
          )

        def validateSubmission(
          pid: Program.Id,
          newStatus: ProposalStatus
        ): Result[Unit] =
          val unmatchedPartners = requestedPartners -- availablePartners
          (
            missingProposal(pid).asFailure.unlessA(hasProposal),
            missingCfP(pid).asFailure.unlessA(cfp.isDefined),
            missingSemester(pid).asFailure.unlessA(semester.isDefined),
            // A Gemini proposal must have a science subtype; an external proposal
            // has none.
            missingScienceSubtype(pid).asFailure.unlessA(isExternal || scienceSubtype.isDefined),
            // Defense in depth: the DB trigger also forbids this, but reject a
            // both-set time request here with a clear message rather than
            // silently treating it as an exchange request below.
            bothTimeRequests(pid).asFailure.whenA(exchangePartner.isDefined && splitsSum =!= 0),
            // Time may only be requested on behalf of a community the call invites.
            exchangePartner.filterNot(_ => exchangeOffered).fold(Result.unit): xp =>
              unofferedExchangePartner(pid, xp).asFailure,
            scienceSubtype.fold(().success) { s =>
              // An exchange-partner time request carries no Gemini partner
              // splits, so the sum-to-100 rule does not apply to it.
              missingOrInvalidSplits(pid, s).asFailure.whenA(
                exchangePartner.isEmpty &&
                splitsSum =!= 100 &&
                ((s === ScienceSubtype.Classical) ||
                 (s === ScienceSubtype.Queue))
              )
            },
            // An external (exchange) proposal apportions its time across Gemini
            // partners, which must sum to 100% at submission.
            missingOrInvalidSplitsExternal(pid).asFailure.whenA(isExternal && splitsSum =!= 100),
            missingConsiderForBand3(pid).asFailure
              .whenA(scienceSubtype.contains(ScienceSubtype.Queue) && considerForBand3.contains(ConsiderForBand3.Unset)),
            missingPartners(pid, unmatchedPartners).asFailure.unlessA(unmatchedPartners.isEmpty),
            validatePiEmailAddress(pid)
          ).tupled.unlessA(newStatus === ProposalStatus.NotSubmitted)

        def validateDeadline(
          pid: Program.Id,
          newStatus: ProposalStatus
        ): Result[Unit] =
          (
            for
              _ <- missingDeadline(pid).asFailure.unlessA(deadline.isDefined)
              _ <- pastDeadline(pid).asFailure.whenA(isPastDeadline.exists(identity))
            yield ()
          ).whenA(newStatus === ProposalStatus.Submitted)

        def updateProgram(
          pid:            Program.Id,
          newType:        Option[ScienceSubtype],
          newObservatory: Observatory,
          newSubaru:      Option[SubaruCallForProposalsType],
          newSemester:    Option[Semester],
          newProprietary: Option[NonNegInt]
        ): F[Unit] =
          val newProgramType: ProgramType = newObservatory match
            case Observatory.Gemini => ProgramType.Science
            case Observatory.Keck   => ProgramType.Keck
            case Observatory.Subaru => ProgramType.Subaru
          session
            .execute(Statements.UpdateProgram)(pid, newType, newProgramType, newSubaru, newSemester, newProprietary)
            .whenA(
              scienceSubtype =!= newType ||
              !observatory.contains(newObservatory) ||
              subaruProposalType =!= newSubaru ||
              newSemester.exists(s => semester.forall(_ =!= s)) ||
              newProprietary.exists(p => proprietary =!= p)
            )

        def edit(set: ProposalPropertiesInput.Edit)(using Transaction[F]): F[Result[ProposalContext]] =
          val eCfp = set.callId.fold(
            ResultT.pure(none[CfpProperties]),              // delete
            ResultT.pure(cfp),                              // don't change
            id => ResultT(lookupProperties(id)).map(_.some) // update if possible
          )

          val eSum =
            if set.hasType then
              set.partnerSplits.fold(0.toLong, splitsSum, m => m.values.map(_.value.toLong).sum)
            else splitsSum

          val newSubtype =
            if set.gemini.isDefined then set.scienceSubtype
            else if set.hasType then none           // switching to keck/subaru
            else scienceSubtype

          // set.observatory is None when the edit doesn't change the variant.
          val newObservatory = set.observatory.orElse(observatory)

          (for
            c <- eCfp
            s <- eCfp.map(_.map(_.semester))
            p <- eCfp.map(_.flatMap(_.gemini).map(_.proprietary).getOrElse(proprietary))
          yield copy(semester = s, scienceSubtype = newSubtype, observatory = newObservatory, splitsSum = eSum, proprietary = p, cfp = c)).value

        private val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MMM-dd")
        private def formatDate(t: Timestamp): String = dateFormatter.format(t.toLocalDateTime)

        private val timeFormatter = DateTimeFormatter.ofPattern("HH:mm")
        private def formatTime(t: Timestamp): String = timeFormatter.format(t.toLocalDateTime)

        private def programUrl(newReference: ProposalReference): Uri = emailConfig.exploreUrl / newReference.label

        private def textSubmissionEmail(newReference: ProposalReference): NonEmptyString = NonEmptyString.unsafeFrom(
          s"""Hello,
          |
          |Thanks for submitting a Gemini proposal!
          |
          |This email confirms that your proposal was received on ${formatDate(currentTime)} at ${formatTime(currentTime)} UT.
          |
          |Call for Proposals: ${cfpTitle.getOrElse("<Missing>")}
          |Proposal Id: ${newReference.label} (${programUrl(newReference)})
          |Proposal Title: ${title.getOrElse("<Missing>")}
          |
          |This proposal may be revised until the CfP deadline on ${deadline.fold("<Missing>")(formatDate)} at ${deadline.fold("<Missing>")(formatTime)} UT.
          |
          |If you have any questions or concerns, please submit a request to the Gemini Help Desk: https://www.gemini.edu/observing/helpdesk/submit-general-helpdesk-request
          |
          |Regards,
          |Gemini Observatory
          """.stripMargin
        )

        private def htmlSubmissionEmail(newReference: ProposalReference): NonEmptyString = NonEmptyString.unsafeFrom(
          s"""|Hello,<br/>
          |<br/>
          |Thanks for submitting a Gemini proposal!<br/>
          |<br/>
          |This email confirms that your proposal was received on ${formatDate(currentTime)} at ${formatTime(currentTime)} UT.<br/>
          |<br/>
          |Call for Proposals: ${escapeHtml(cfpTitleText)}<br/>
          |Proposal Id: <a href="${programUrl(newReference)}">${newReference.label}</a><br/>
          |Proposal Title: ${escapeHtml(titleText)}<br/>
          |<br/>
          |This proposal may be revised until the CfP deadline on ${deadline.fold("<Missing>")(formatDate)} at ${deadline.fold("<Missing>")(formatTime)} UT.<br/>
          |<br/>
          |If you have any questions or concerns, please submit a request to the <a href="https://www.gemini.edu/observing/helpdesk/submit-general-helpdesk-request">Gemini Help Desk</a><br/>
          |<br/>
          |Regards,<br/>
          |Gemini Observatory
          """.stripMargin
        )

        private def emailSubject(newReference: ProposalReference): NonEmptyString = NonEmptyString.unsafeFrom(
          s"Gemini Proposal ${newReference.label}"
        )

        private def getNewReference(pid: Program.Id): F[Result[ProposalReference]] =
         // A proposal reference is generated by the database, so we should never fail to get one
         session.unique(Statements.SelectProposalReference)(pid)
           .map(_.fold(OdbError.UpdateFailed("System error: could not generate proposal reference".some).asFailure)(_.success))

        private def sendEmailHelper(
          pid: Program.Id,
          recipient: EmailAddress,
          subject: NonEmptyString,
          text: NonEmptyString,
          html: Option[NonEmptyString]
        )(using Transaction[F]): F[Result[Unit]] =
          Services.asSuperUser:
            emailService
              .send(pid, emailConfig.invitationFrom, recipient, subject, text, html)
              .map(_ => Result.unit)

        def sendSubmissionEmail(pid: Program.Id, newReference: ProposalReference)(using Transaction[F]): F[Result[Unit]] =
          piEmailAddress // this has already been validated, so we should have one
            .fold(Result.unit.pure)(email =>
              sendEmailHelper(pid, email, emailSubject(newReference), textSubmissionEmail(newReference), htmlSubmissionEmail(newReference).some)
            )

        // Addresses notified when a proposal is submitted, in addition to the PI.  An
        // external (Keck/Subaru) proposal has no Gemini call type and is not announced.
        // Duplicates are removed because several of the configured addresses may be the
        // same, in particular when they all fall back to PROPOSAL_EMAIL_DEFAULT.
        private lazy val notificationRecipients: List[EmailAddress] =
          geminiCallType.toList.flatMap {
            case GeminiCallForProposalsType.RegularSemester =>
              // A regular semester proposal requests time either from an exchange partner
              // or from the Gemini partners named in its splits, never both.
              exchangePartner.fold(
                requestedPartners.toList.sortBy(_.tag).map(emailConfig.proposalEmails.forPartner)
              )(p => List(emailConfig.proposalEmails.forExchangePartner(p)))
            case callType                                   =>
              emailConfig.proposalEmails.forCfpType(callType).toList
          }.distinct

        private def notificationSubject(newReference: ProposalReference): NonEmptyString =
          NonEmptyString.unsafeFrom(s"New $typeText proposal received: ${newReference.label}")

        private def notificationText(
          newReference: ProposalReference,
          timeRequested: Option[CalculatedValue[CategorizedTimeRange]]
        ): NonEmptyString = NonEmptyString.unsafeFrom(
          s"""|A new $typeText proposal has been received:
              |Id: ${newReference.label}
              |URL: ${programUrl(newReference)}
              |Title: $titleText
              |PI: $piNameText ($piObservatory)
              |CoIs: $coiNamesText
              |Request: ${timeRequestedText(timeRequested)}
              |Instruments: $instrumentsText
              |Abstract: $abstractText""".stripMargin
        )

        private def notificationHtml(
          newReference: ProposalReference,
          timeRequested: Option[CalculatedValue[CategorizedTimeRange]]
        ): NonEmptyString = NonEmptyString.unsafeFrom(
          s"""|A new $typeText proposal has been received:<br/>
              |Id: ${newReference.label}<br/>
              |URL: <a href="${programUrl(newReference)}">${programUrl(newReference)}</a><br/>
              |Title: ${escapeHtml(titleText)}<br/>
              |PI: ${escapeHtml(piNameText)} ($piObservatory)<br/>
              |CoIs: ${escapeHtml(coiNamesText)}<br/>
              |Request: ${timeRequestedText(timeRequested)}<br/>
              |Instruments: $instrumentsText<br/>
              |Abstract: ${escapeHtml(abstractText)}""".stripMargin
        )

        def sendNotificationEmails(pid: Program.Id, newReference: ProposalReference)(using Transaction[F]): F[Result[Unit]] =
          notificationRecipients match
            case Nil        => Result.unit.pure
            case recipients =>
              (for {
                timeRequested <- ResultT.liftF(timeRequest(pid))
                _             <- recipients.traverse: a =>
                                   ResultT(sendEmailHelper(
                                     pid,
                                     a,
                                     notificationSubject(newReference),
                                     notificationText(newReference, timeRequested),
                                     notificationHtml(newReference, timeRequested).some
                                   ))
              } yield ()).value

        def sendEmail(
          pid: Program.Id,
          newStatus: ProposalStatus
         )(using Transaction[F]): F[Result[Unit]] =
          // There might be emails for other status changes in the future
          if newStatus === ProposalStatus.Submitted then
            (for {
              newReference <- ResultT(getNewReference(pid))
              _            <- ResultT(sendSubmissionEmail(pid, newReference))
              _            <- ResultT(sendNotificationEmails(pid, newReference))
            } yield ()).value
          else Result.unit.pure

      }

      object ProposalContext {
        val parts: Decoder[Set[Partner]] =
          _partner.map(_.toList.toSet)

        val instrumentList: Decoder[List[Instrument]] =
          _instrument.map(_.toList)

        val codec: Decoder[ProposalContext] =
          (proposal_status *: bool *: varchar_nonempty.opt *: text.opt *: text_nonempty.opt *: text.opt *: proposal_reference.opt *: semester.opt *: science_subtype.opt *: int8 *: parts *: parts *: text_list *: instrumentList *: int4_nonneg *: core_timestamp *: core_timestamp.opt *: text_nonempty.opt *: CallForProposalsService.Statements.cfp_properties.opt *: consider_for_band_3.opt *: exchange_partner.opt *: bool *: observatory.opt).to[ProposalContext]

        def lookup(pid: Program.Id): F[Result[ProposalContext]] =
          val af = Statements.selectProposalContext(user, pid)
          session.prepareR(
            af.fragment.query(program_type *: codec)
          ).use { ps =>
            ps.option(af.argument).map {
              case Some((t, pc)) if t.hasProposal => pc.success
              case Some((t, pc))                  => invalidProgramType(pid, t).asFailure
              case _                               => OdbError.InvalidProgram(pid).asFailure
            }
          }

      }

      def deferConstraints: F[Unit] =
        session.execute(sql"SET CONSTRAINTS ALL DEFERRED".command).void

      def createProposal(
        input: CreateProposalInput
      )(using Transaction[F], Services.PiAccess): F[Result[Program.Id]] = {

        def lookupCfpProperties: ResultT[F, Option[CfpProperties]] =
          input.SET.callId.traverse(cid => ResultT(lookupProperties(cid)))

        // Make sure the indicated CfP is compatible with the inputs: the
        // observatory must match, and (for Gemini/Subaru) the science subtype or
        // Subaru call type must agree with the call.
        def checkCfpCompatibility(o: Option[CfpProperties]): ResultT[F, Unit] =
          ResultT.fromResult(o.fold(Result.unit)(_.validate(input.SET.observatory, input.SET.scienceSubtype)))

        // Update the program's science subtype and/or semester to match inputs.
        def updateProgram(
          p: ProposalContext,
          c: Option[CfpProperties]
        ): ResultT[F, Unit] =
          ResultT.liftF(p.updateProgram(input.programId, input.SET.scienceSubtype, input.SET.observatory, c.flatMap(_.subaruProposalType), c.map(_.semester), c.flatMap(_.gemini).map(_.proprietary)))

        val insert: ResultT[F, Unit] =
          val af = Statements.insertProposal(input.programId, input.SET)
          val create = session.prepareR(af.fragment.command).use(_.execute(af.argument).void)
          ResultT(create.map(_.success).recover {
            case SqlState.UniqueViolation(e) =>
              error.creationFailed(input.programId).asFailure
            case SqlState.CheckViolation(e) if e.constraintName == Some("chk_reviewer_mentor_different") =>
              OdbError.InvalidArgument("The same user cannot be both reviewer and mentor on a proposal".some).asFailure
            case SqlState.RaiseException(ex) =>
              OdbError.InvalidArgument(ex.message.some).asFailure
          })

        val insertSplits: ResultT[F, Unit] =
          ResultT.liftF(
            Services.asSuperUser:
              partnerSplitsService.insertSplits(input.SET.partnerSplits, input.programId)
          )

        (for {
          c <- lookupCfpProperties
          _ <- checkCfpCompatibility(c)
          p <- ResultT(ProposalContext.lookup(input.programId))
          _ <- ResultT.liftF(deferConstraints)
          _ <- updateProgram(p, c)
          _ <- insert
          _ <- insertSplits
        } yield input.programId).value

      }

      def updateProposal(
        input: UpdateProposalInput
      )(using Transaction[F], Services.PiAccess): F[Result[Program.Id]] = {

        // Make sure the indicated CfP is compatible with the proposal: the
        // observatory must match, and (for Gemini/Subaru) the science subtype or
        // Subaru call type must agree with the call.
        def checkCfpCompatibility(p: ProposalContext): ResultT[F, Unit] =
          ResultT.fromResult(p.cfp.fold(Result.unit)(_.validate(p.obs, p.scienceSubtype)))

        def checkUserAccess(pid: Program.Id, p: ProposalContext): ResultT[F, Unit] =
          ResultT.fromResult(
            cannotEditSubmittedProposal(pid, user).asFailure.unlessA(p.status.userCanEditProposal)
          )

        // Update the program's science subtype and/or semester to match inputs.
        def updateProgram(pid: Program.Id, before: ProposalContext, after: ProposalContext): ResultT[F, Unit] =
          ResultT.liftF(before.updateProgram(pid, after.scienceSubtype, after.obs, after.subaruProposalType, after.semester, after.proprietary.some))

        // When the proposal-type variant changes (a different Gemini science
        // subtype, or a switch to/from external), expand the edit into a full
        // "create as edit" so that all the variant's columns are (re)set.
        // A change to a Gemini proposal whose subtype differs from the current one
        // (including a switch from an external proposal, which has no subtype) is
        // expanded into a full "create as edit" so every Gemini column is (re)set.
        // The external branch needs no expansion: its update statement always
        // resets the Gemini-specific columns.
        def handleTypeChange(before: ProposalContext): ProposalPropertiesInput.Edit =
          input.SET.gemini
            .filterNot(c => before.scienceSubtype.exists(_ === c.scienceSubtype))
            .fold(input.SET)(call => input.SET.copy(gemini = call.asCreate.asEdit.some))

        def updateProposal(pid: Program.Id, set: ProposalPropertiesInput.Edit): ResultT[F, Unit] =
          ResultT(Statements.updateProposal(pid, set).fold(().success.pure[F]) { af =>
            session
              .prepareR(af.fragment.command)
              .use(_.execute(af.argument))
              .map {
                case Update(1) => ().success
                case _         => error.updateFailed(pid).asFailure
              }
              .recover {
                case SqlState.CheckViolation(e) if e.constraintName == Some("chk_reviewer_mentor_different") =>
                  OdbError.InvalidArgument("The same user cannot be both reviewer and mentor on a proposal".some).asFailure
                case SqlState.RaiseException(ex) =>
                  OdbError.InvalidArgument(ex.message.some).asFailure
              }
          })

        def updateSplits(pid: Program.Id, set: ProposalPropertiesInput.Edit): ResultT[F, Unit] =
          ResultT.liftF(set.partnerSplits.foldPresent( splits =>
            Services.asSuperUser:
              partnerSplitsService.updateSplits(splits.getOrElse(Map.empty), pid)
          ).sequence.void)

        // The time-request trigger fires immediately and rejects a proposal that has
        // both an exchange partner and partner splits, so an edit that swaps one for
        // the other has to give up what it holds before taking on the other.  Only
        // assigning an exchange partner needs the splits emptied first; going the
        // other way clears the exchange partner with the proposal update, ahead of
        // the splits that replace it.
        def splitsFirst(set: ProposalPropertiesInput.Edit): Boolean =
          set.exchangePartner.isPresent

        (for {
          pid    <- ResultT(programService.resolvePid(input.programId, input.proposalReference, input.programReference))
          before <- ResultT(ProposalContext.lookup(pid))
          after  <- ResultT(before.edit(input.SET))
          _      <- checkCfpCompatibility(after)
          _      <- checkUserAccess(pid, after)
          _      <- ResultT.fromResult(after.validateSubmission(pid, after.status))
          _      <- ResultT.liftF(deferConstraints)
          set     = handleTypeChange(before)
          _      <- updateSplits(pid, set).whenA(splitsFirst(set))
          _      <- updateProposal(pid, set)
          _      <- updateProgram(pid, before, after)
          _      <- updateSplits(pid, set).unlessA(splitsFirst(set))
        } yield pid).value
      }

      override def timeRequest(pid: Program.Id)(using Transaction[F]): F[Option[CalculatedValue[CategorizedTimeRange]]] =
        session.option(Statements.SelectTimeRequest)(pid).map(_.flatten).flatMap:
          case Some(ts) => explicitTimeRequestRange(ts).some.pure[F]
          case None     => timeEstimateService.estimateProgramRange(pid)

      override def hasProposal(pid: Program.Id)(using Transaction[F]): F[Boolean] =
        session.unique(Statements.HasProposal)(pid)

      override def deleteProposal(
        input: DeleteProposalInput
      )(using Transaction[F], Services.StaffAccess): F[Result[Boolean]] =
        session
          .execute(Statements.DeleteProposal)(input.programId)
          .flatMap {
            case Delete(0) => false.success.pure[F]
            case Delete(1) =>
              session.execute(Statements.ResetProgramTypeToScience)(input.programId).as(true.success)
            case c         => OdbError.InvalidArgument(s"Could not delete proposal in ${input.programId}: $c".some).asFailure.pure[F]
          }

      override def setProposalStatus(
        input: SetProposalStatusInput,
        commitHash: CommitHash,
        itcClient: ItcClient[F],
        ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
      )(using NoTransaction[F], Services.PiAccess): F[Result[Program.Id]] = {

        def validate(
          pid: Program.Id,
          ctx: ProposalContext,
          states: Set[ObservationWorkflowState],
          oldStatus: ProposalStatus,
          newStatus: ProposalStatus
        ): Result[Unit] =
          for {
            _ <- undefinedObservations(pid).asFailure.whenA(states.contains(ObservationWorkflowState.Undefined))
            _ <- notAuthorizedNew(pid, user, newStatus).asFailure.unlessA(newStatus.userCanChangeStatus)
            _ <- notAuthorizedOld(pid, user, ctx.status).asFailure.unlessA(oldStatus.userCanChangeStatus)
            _ <- ctx.validateSubmission(pid, newStatus)
            _ <- ctx.validateDeadline(pid, newStatus)
          } yield ()

        def update(pid: Program.Id, ps: ProposalStatus): F[Unit] =
          val af = Statements.updateProposalStatus(user, pid, ps)
          session.prepareR(af.fragment.command).use(_.execute(af.argument)).void

        // On acceptance, freeze the ToO ceiling.  Until now an unset ceiling is
        // derived as the maximum activation among the program's observations,
        // which is only safe while the proposal is under review: left live, a PI
        // could raise their own ceiling afterwards just by adding an interrupting
        // observation.  Materializing the effective value here turns a
        // description of what was proposed into an authorization.  The TAC
        // approves it implicitly by leaving it alone, or explicitly by editing it
        // before accepting -- either way, what is frozen is what they saw.
        def freezeTooActivation(pid: Program.Id): F[Unit] =
          session.prepareR(Statements.FreezeTooActivation).use(_.execute(pid)).void

        ResultT(programService.resolvePid(input.programId, input.proposalReference, input.programReference))
          .flatMap: pid =>
            ResultT(Services.asSuperUser(observationWorkflowService.getWorkflows(pid))).flatMap: wfs =>
              val states = wfs.values.map(_.state).toSet
              ResultT:
                services.transactionally:
                  val go2 =
                    for
                      info      <- ResultT(ProposalContext.lookup(pid))
                      oldStatus  = info.status
                      newStatus  = input.status
                      _         <- ResultT.fromResult(validate(pid, info, states, oldStatus, newStatus))
                      _         <- ResultT.liftF(update(pid, input.status))
                      _         <- ResultT(configurationService.canonicalizeAll(pid)).whenA(oldStatus === ProposalStatus.NotSubmitted && newStatus === ProposalStatus.Submitted)
                      _         <- ResultT(configurationService.deleteAll(pid)).whenA(oldStatus === ProposalStatus.Submitted && newStatus === ProposalStatus.NotSubmitted)
                      _         <- ResultT.liftF(freezeTooActivation(pid)).whenA(newStatus === ProposalStatus.Accepted)
                      _         <- ResultT(info.sendEmail(pid, newStatus))
                    yield pid
                  go2.value
          .value

      }
    }

  private object Statements {

    val SelectTimeRequest: Query[Program.Id, Option[TimeSpan]] =
      sql"""
        SELECT c_time_request FROM t_proposal WHERE c_program_id = $program_id
      """.query(time_span.opt)

    val HasProposal: Query[Program.Id, Boolean] =
      sql"""
        SELECT COUNT(1) FROM t_proposal WHERE c_program_id = $program_id
      """.query(int8.map(_ >= 1))

    val DeleteProposal: Command[Program.Id] =
      sql"""
        DELETE FROM t_proposal WHERE c_program_id = $program_id
      """.command

    // Removing a proposal reverts an exchange (keck/subaru) program back to a
    // plain Gemini science program.
    val ResetProgramTypeToScience: Command[Program.Id] =
      sql"""
        UPDATE t_program
           SET c_program_type         = 'science',
               c_subaru_proposal_type = NULL
         WHERE c_program_id = $program_id
           AND c_program_type IN ('keck', 'subaru')
      """.command

    def updates(SET: ProposalPropertiesInput.Edit): Option[NonEmptyList[AppliedFragment]] = {
      val mainUpdates: List[AppliedFragment] =
        List(
          SET.category.foldPresent(sql"c_category = ${tag.opt}"),
          SET.callId.foldPresent(sql"c_cfp_id = ${cfp_id.opt}"),
          SET.timeRequest.foldPresent(sql"c_time_request = ${time_span.opt}")
        ).flatten

      val geminiUpdates: List[AppliedFragment] =
        SET.gemini.toList.flatMap { call =>
          // reset consider_for_band_3 for classical proposals.
          val considerForBand3Update =
            if call.scienceSubtype === ScienceSubtype.Classical then
              sql"c_consider_for_band_3 = ${consider_for_band_3}"(ConsiderForBand3.Unset).some
            else
              call.considerForBand3.map(sql"c_consider_for_band_3 = ${consider_for_band_3}")

          // A Gemini proposal sits at Gemini.
          sql"c_observatory = ${observatory}"(Observatory.Gemini) ::
          sql"c_science_subtype = $science_subtype"(call.scienceSubtype) ::
          List(
            call.tooActivationCeiling.foldPresent(sql"c_too_activation = ${too_activation.opt}"),
            call.minPercentTime.map(sql"c_min_percent = ${int_percent}"),
            call.minPercentTotal.foldPresent(sql"c_min_percent_total = ${int_percent.opt}"),
            call.totalTime.foldPresent(sql"c_total_time = ${time_span.opt}"),
            call.reviewerId.foldPresent(sql"c_reviewer_id = ${program_user_id.opt}"),
            call.mentorId.foldPresent(sql"c_mentor_id = ${program_user_id.opt}"),
            call.aeonMultiFacility.map(sql"c_aeon_multi_facility = ${bool}"),
            call.jwstSynergy.map(sql"c_jwst_synergy = ${bool}"),
            call.usLongTerm.map(sql"c_us_long_term = ${bool}"),
            call.exchangePartner.foldPresent(sql"c_exchange_partner = ${exchange_partner.opt}"),
            considerForBand3Update
          ).flatten
        }

      // An external (Keck/Subaru) proposal carries an observatory and clears all
      // Gemini-specific properties.  (The Subaru proposal type is a property of
      // the call, not the proposal.)
      val externalUpdates: List[AppliedFragment] =
        if SET.keck.isEmpty && SET.subaru.isEmpty then Nil
        else {
          val obs: Observatory =
            if SET.subaru.isDefined then Observatory.Subaru else Observatory.Keck
          // The minimum percent time comes from whichever exchange type is
          // present; when omitted the existing value is left unchanged.
          val minPercentTime: Option[IntPercent] =
            SET.keck.flatMap(_.minPercentTime).orElse(SET.subaru.flatMap(_.minPercentTime))
          List(
            sql"c_observatory = ${observatory}"(obs).some,
            sql"c_science_subtype = ${science_subtype.opt}"(none).some,
            // An exchange proposal cannot have ToO at all, so clear the explicit
            // ceiling rather than leaving a stale one behind.  The derivation is
            // capped to 'none' for these types anyway.
            sql"c_too_activation = ${too_activation.opt}"(none).some,
            minPercentTime.map(sql"c_min_percent = ${int_percent}"),
            sql"c_min_percent_total = ${int_percent.opt}"(none).some,
            sql"c_total_time = ${time_span.opt}"(none).some,
            sql"c_reviewer_id = ${program_user_id.opt}"(none).some,
            sql"c_mentor_id = ${program_user_id.opt}"(none).some,
            sql"c_aeon_multi_facility = ${bool}"(false).some,
            sql"c_jwst_synergy = ${bool}"(false).some,
            sql"c_us_long_term = ${bool}"(false).some,
            sql"c_consider_for_band_3 = ${consider_for_band_3}"(ConsiderForBand3.Unset).some,
            sql"c_exchange_partner = ${exchange_partner.opt}"(none).some
          ).flatten
        }

      NonEmptyList.fromList(mainUpdates ++ geminiUpdates ++ externalUpdates)
    }

    def updateProposal(pid: Program.Id, SET: ProposalPropertiesInput.Edit): Option[AppliedFragment] =
      updates(SET).map { us =>
        void"""
          UPDATE t_proposal
          SET """ |+| us.intercalate(void", ") |+|
        sql"""
          WHERE t_proposal.c_program_id = $program_id
        """.apply(pid)
      }

    /** Insert a proposal. */
    def insertProposal(pid: Program.Id, c: ProposalPropertiesInput.Create): AppliedFragment =
      c.gemini match
        case Some(g) => insertGeminiProposal(pid, c, g)
        case None    =>
          // Keck or Subaru exchange proposal (or, defensively, the Gemini default).
          if c.keck.isDefined || c.subaru.isDefined then insertExternalProposal(pid, c)
          else insertGeminiProposal(pid, c, GeminiProposalTypeInput.Create.Default)

    private def insertGeminiProposal(
      pid: Program.Id,
      c:   ProposalPropertiesInput.Create,
      g:   GeminiProposalTypeInput.Create
    ): AppliedFragment =
      sql"""
        INSERT INTO t_proposal (
          c_program_id,
          c_cfp_id,
          c_category,
          c_observatory,
          c_science_subtype,
          c_too_activation,
          c_min_percent,
          c_min_percent_total,
          c_total_time,
          c_reviewer_id,
          c_mentor_id,
          c_aeon_multi_facility,
          c_jwst_synergy,
          c_us_long_term,
          c_consider_for_band_3,
          c_exchange_partner,
          c_time_request
        ) SELECT
          ${program_id},
          ${cfp_id.opt},
          ${tag.opt},
          ${observatory},
          ${science_subtype},
          ${too_activation.opt},
          ${int_percent},
          ${int_percent.opt},
          ${time_span.opt},
          ${program_user_id.opt},
          ${program_user_id.opt},
          ${bool},
          ${bool},
          ${bool},
          ${consider_for_band_3},
          ${exchange_partner.opt},
          ${time_span.opt}
      """.apply(
        pid,
        c.callId,
        c.category,
        Observatory.Gemini,
        g.scienceSubtype,
        g.tooActivationCeiling,
        g.minPercentTime,
        g.minPercentTotal,
        g.totalTime,
        g.reviewerId,
        g.mentorId,
        g.aeonMultiFacility,
        g.jwstSynergy,
        g.usLongTerm,
        g.considerForBand3,
        g.exchangePartner,
        c.timeRequest
      )

    // An external (exchange) proposal has no science subtype; it carries an
    // observatory instead.  The Gemini-specific columns take their table
    // defaults; c_min_percent has no default and is taken from the exchange
    // proposal type (defaulting to 100%).
    private def insertExternalProposal(
      pid: Program.Id,
      c:   ProposalPropertiesInput.Create
    ): AppliedFragment =
      val minPercentTime =
        c.keck.map(_.minPercentTime)
          .orElse(c.subaru.map(_.minPercentTime))
          .getOrElse(IntPercent.unsafeFrom(100))
      sql"""
        INSERT INTO t_proposal (
          c_program_id,
          c_cfp_id,
          c_category,
          c_observatory,
          c_min_percent,
          c_time_request
        ) SELECT
          ${program_id},
          ${cfp_id.opt},
          ${tag.opt},
          ${observatory},
          ${int_percent},
          ${time_span.opt}
      """.apply(
        pid,
        c.callId,
        c.category,
        c.observatory,
        minPercentTime,
        c.timeRequest
      )

    // The science subtype, program type (which encodes the observatory:
    // gemini->science, keck->keck, subaru->subaru) and Subaru proposal type are
    // mirrored from the proposal (so they may be cleared), while the semester and
    // proprietary period (which come from the CfP) are left unchanged when absent.
    val UpdateProgram: Command[(Program.Id, Option[ScienceSubtype], ProgramType, Option[SubaruCallForProposalsType], Option[Semester], Option[NonNegInt])] =
      sql"""
        UPDATE t_program
           SET c_science_subtype      = ${science_subtype.opt},
               c_program_type         = ${program_type},
               c_subaru_proposal_type = ${subaru_proposal_type.opt},
               c_semester        = CASE
                                     WHEN ${semester.opt} IS NULL THEN c_semester
                                     ELSE ${semester.opt}
                                   END,
               c_goa_proprietary = CASE
                                     WHEN ${int4_nonneg.opt} is NULL THEN c_goa_proprietary
                                     ELSE ${int4_nonneg.opt}
                                   END
         WHERE c_program_id = $program_id
      """.command.contramap { case (p, t, o, su, s, r) => (t, o, su, s, s, r, r, p) }

    def selectProposalContext(user: User, pid: Program.Id): AppliedFragment =
      sql"""
        SELECT
          prog.c_program_type,
          prog.c_proposal_status,
          prop.c_program_id IS NOT NULL,
          pi.c_email,
          pi.c_display_name,
          prog.c_name,
          prog.c_description,
          prog.c_proposal_reference,
          prog.c_semester,
          prog.c_science_subtype,
          COALESCE(
            (SELECT SUM(c_percent) FROM t_partner_split WHERE c_program_id = prog.c_program_id),
            0
          ) AS c_splits_sum,
          COALESCE(
            (SELECT
               ARRAY_AGG(DISTINCT
                 CASE
                   WHEN c_partner_link = 'has_non_partner' THEN 'us'::d_tag
                   ELSE c_gemini_partner
                 END
               )
             FROM t_program_user
             WHERE c_program_id = prog.c_program_id
               AND (c_gemini_partner IS NOT NULL OR c_partner_link = 'has_non_partner')
            ),
            '{}'
          ) AS c_available_partners,
          COALESCE(
            (SELECT ARRAY_AGG(DISTINCT c_partner) FROM t_partner_split WHERE c_program_id = prog.c_program_id AND c_percent > 0),
            '{}'
          ) AS c_requested_partners,
          COALESCE(
            (SELECT ARRAY_AGG(pu.c_display_name ORDER BY pu.c_display_name)
             FROM v_program_user pu
             WHERE pu.c_program_id = prog.c_program_id
               AND pu.c_role IN ('coi', 'coi_ro')
               AND pu.c_display_name IS NOT NULL
            ),
            '{}'
          ) AS c_coi_names,
          COALESCE(
            (SELECT ARRAY_AGG(DISTINCT obs.c_instrument)
             FROM t_observation obs
             WHERE obs.c_program_id = prog.c_program_id
               AND obs.c_existence = 'present'
               AND obs.c_workflow_user_state IS DISTINCT FROM 'inactive'
               AND obs.c_calibration_role IS NULL
               AND obs.c_instrument IS NOT NULL
            ),
            '{}'
          ) AS c_instruments,
          prog.c_goa_proprietary,
          LOCALTIMESTAMP,
          COALESCE(
            cfp_pi.c_deadline,
            (SELECT cfp.c_gemini_non_partner_deadline
             WHERE pi.c_partner_link = 'has_non_partner'),
            -- An exchange-partner request is not tied to any Gemini partner, so
            -- it uses that community's deadline for the call: its override if it
            -- has one, and otherwise the call's default.  Null when the call does
            -- not offer the community at all.
            cfp_ep.c_deadline
          ) AS c_deadline,
          cfp.c_title,
          cfp.c_cfp_id,
          cfp.c_semester,
          cfp.c_observatory,
          cfp.c_subaru_proposal_type,
          cfp.c_gemini_proposal_type,
          cfp.c_gemini_proprietary,
          prop.c_consider_for_band_3,
          prop.c_exchange_partner,
          cfp_ep.c_cfp_id IS NOT NULL AS c_exchange_offered,
          prop.c_observatory
        FROM t_program prog
        LEFT JOIN t_proposal prop
          ON prog.c_program_id = prop.c_program_id
        LEFT JOIN v_cfp cfp
          ON prop.c_cfp_id = cfp.c_cfp_id
        LEFT JOIN v_program_user pi
          ON prog.c_program_id = pi.c_program_id
          AND pi.c_role = 'pi'
        LEFT JOIN v_gemini_cfp_partner cfp_pi
          ON cfp.c_cfp_id = cfp_pi.c_cfp_id
          AND cfp_pi.c_partner = pi.c_gemini_partner
        LEFT JOIN v_gemini_cfp_exchange_partner cfp_ep
          ON cfp.c_cfp_id = cfp_ep.c_cfp_id
          AND cfp_ep.c_exchange_partner = prop.c_exchange_partner
        WHERE
          prog.c_program_id = $program_id
      """.apply(pid) |+|
      ProgramUserService.Statements.andWhereUserReadAccess(user, pid)

    /**
     * Materializes the effective ToO ceiling into the proposal.  The `IS NULL`
     * guard makes this a no-op when a ceiling was chosen explicitly (it is
     * already concrete) and makes re-running harmless.  Reading v_proposal while
     * updating t_proposal is safe: the FROM sees the pre-update snapshot.
     */
    val FreezeTooActivation: Command[Program.Id] =
      sql"""
        UPDATE t_proposal p
           SET c_too_activation = v.c_too_activation_effective
          FROM v_proposal v
         WHERE v.c_program_id = p.c_program_id
           AND p.c_program_id = $program_id
           AND p.c_too_activation IS NULL
      """.command

    def updateProposalStatus(user: User, pid: Program.Id, status: ProposalStatus): AppliedFragment =
      sql"""
        UPDATE t_program
        SET c_proposal_status = $proposal_status
        WHERE c_program_id = $program_id
      """.apply(status, pid) |+|
      ProgramUserService.Statements.andWhereUserWriteAccess(user, pid)

    val SelectProposalReference: Query[Program.Id, Option[ProposalReference]] =
      sql"""
        SELECT c_proposal_reference
        FROM t_program
        WHERE c_program_id = $program_id
      """.query(proposal_reference.opt)

  }
}
