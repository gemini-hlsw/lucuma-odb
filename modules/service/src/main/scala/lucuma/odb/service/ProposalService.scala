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
import lucuma.core.enums.ConsiderForBand3
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.Observatory
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ProposalStatus
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.SubaruCallForProposalsType
import lucuma.core.enums.ToOActivation
import lucuma.core.model.Access
import lucuma.core.model.CallForProposals
import lucuma.core.model.Program
import lucuma.core.model.IntPercent
import lucuma.core.model.ProposalReference
import lucuma.core.model.Semester
import lucuma.core.model.User
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
        title:             Option[NonEmptyString],
        reference:         Option[ProposalReference],
        semester:          Option[Semester],
        scienceSubtype:    Option[ScienceSubtype],
        splitsSum:         Long,
        availablePartners: Set[Partner],
        requestedPartners: Set[Partner],
        proprietary:       NonNegInt,
        currentTime:       Timestamp,
        deadline:          Option[Timestamp],
        cfpTitle:          Option[NonEmptyString],
        cfp:               Option[CfpProperties],
        considerForBand3:  Option[ConsiderForBand3],
        exchangePartner:   Option[ExchangePartner],
        observatory:       Option[Observatory],
        subaruProposalType: Option[SubaruCallForProposalsType]
      ) {
        // Every stored proposal has an observatory; default to Gemini defensively.
        val obs: Observatory = observatory.getOrElse(Observatory.Gemini)
        val isExternal: Boolean = obs =!= Observatory.Gemini

        val isPastDeadline: Option[Boolean] =
          deadline.map(_ < currentTime)

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
          session
            .execute(Statements.UpdateProgram)(pid, newType, newObservatory, newSubaru, newSemester, newProprietary)
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

          // A Subaru proposal always has a call type (defaulting to Normal); any
          // other observatory has none.
          val newSubaru =
            newObservatory match
              case Some(Observatory.Subaru) =>
                set.subaruCallType.orElse(subaruProposalType).orElse(SubaruCallForProposalsType.Normal.some)
              case _                        => none

          (for
            c <- eCfp
            s <- eCfp.map(_.map(_.semester))
            p <- eCfp.map(_.flatMap(_.gemini).map(_.proprietary).getOrElse(proprietary))
          yield copy(semester = s, scienceSubtype = newSubtype, observatory = newObservatory, subaruProposalType = newSubaru, splitsSum = eSum, proprietary = p, cfp = c)).value

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
          s"""Hello,<br/>
          <br/>
          |Thanks for submitting a Gemini proposal!<br/>
          |<br/>
          |This email confirms that your proposal was received on ${formatDate(currentTime)} at ${formatTime(currentTime)} UT.<br/>
          |<br/>
          |Call for Proposals: ${cfpTitle.getOrElse("<Missing>")}<br/>
          |Proposal Id: <a href="${programUrl(newReference)}">${newReference.label}</a><br/>
          |Proposal Title: ${title.getOrElse("<Missing>")}<br/>
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

        def sendSubmissionEmail(pid: Program.Id)(using Transaction[F]): F[Result[Unit]] =
          piEmailAddress // this has already been validated, so we should have one
            .fold(Result.unit.pure)(email =>
              (for {
                newReference <- ResultT(getNewReference(pid))
                _            <- ResultT(sendEmailHelper(pid, email, emailSubject(newReference), textSubmissionEmail(newReference), htmlSubmissionEmail(newReference).some))
              } yield ()).value
            )

        def sendEmail(
          pid: Program.Id,
          newStatus: ProposalStatus
         )(using Transaction[F]): F[Result[Unit]] =
          // There might be other emails in the future
          if newStatus === ProposalStatus.Submitted then sendSubmissionEmail(pid)
          else Result.unit.pure

      }

      object ProposalContext {
        val parts: Decoder[Set[Partner]] =
          _partner.map(_.toList.toSet)

        val coNames: Decoder[List[Option[NonEmptyString]]] =
          _text.map(_.toList.map(n => NonEmptyString.from(n).toOption))

        val codec: Decoder[ProposalContext] =
          (proposal_status *: bool *: varchar_nonempty.opt *: text_nonempty.opt *: proposal_reference.opt *: semester.opt *: science_subtype.opt *: int8 *: parts *: parts *: int4_nonneg *: core_timestamp *: core_timestamp.opt *: text_nonempty.opt *: CallForProposalsService.Statements.cfp_properties.opt *: consider_for_band_3.opt *: exchange_partner.opt *: observatory.opt *: subaru_proposal_type.opt).to[ProposalContext]

        def lookup(pid: Program.Id): F[Result[ProposalContext]] =
          val af = Statements.selectProposalContext(user, pid)
          session.prepareR(
            af.fragment.query(program_type *: codec)
          ).use { ps =>
            ps.option(af.argument).map {
              case Some((ProgramType.Science, pc)) => pc.success
              case Some((t, pc))                   => invalidProgramType(pid, t).asFailure
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
          ResultT.fromResult(o.fold(Result.unit)(_.validate(input.SET.observatory, input.SET.scienceSubtype, input.SET.subaruProposalType)))

        // Update the program's science subtype and/or semester to match inputs.
        def updateProgram(
          p: ProposalContext,
          c: Option[CfpProperties]
        ): ResultT[F, Unit] =
          ResultT.liftF(p.updateProgram(input.programId, input.SET.scienceSubtype, input.SET.observatory, input.SET.subaruProposalType, c.map(_.semester), c.flatMap(_.gemini).map(_.proprietary)))

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
          ResultT.fromResult(p.cfp.fold(Result.unit)(_.validate(p.obs, p.scienceSubtype, p.subaruProposalType)))

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

        (for {
          pid    <- ResultT(programService.resolvePid(input.programId, input.proposalReference, input.programReference))
          before <- ResultT(ProposalContext.lookup(pid))
          after  <- ResultT(before.edit(input.SET))
          _      <- checkCfpCompatibility(after)
          _      <- checkUserAccess(pid, after)
          _      <- ResultT.fromResult(after.validateSubmission(pid, after.status))
          _      <- ResultT.liftF(deferConstraints)
          set     = handleTypeChange(before)
          _      <- updateProposal(pid, set)
          _      <- updateProgram(pid, before, after)
          _      <- updateSplits(pid, set)
        } yield pid).value
      }

      override def hasProposal(pid: Program.Id)(using Transaction[F]): F[Boolean] =
        session.unique(Statements.HasProposal)(pid)

      override def deleteProposal(
        input: DeleteProposalInput
      )(using Transaction[F], Services.StaffAccess): F[Result[Boolean]] =
        session
          .execute(Statements.DeleteProposal)(input.programId)
          .map {
            case Delete(0) => false.success
            case Delete(1) => true.success
            case c         => OdbError.InvalidArgument(s"Could not delete proposal in ${input.programId}: $c".some).asFailure
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
                      _         <- ResultT(info.sendEmail(pid, newStatus))
                    yield pid
                  go2.value
          .value

      }
    }

  private object Statements {

    val HasProposal: Query[Program.Id, Boolean] =
      sql"""
        SELECT COUNT(1) FROM t_proposal WHERE c_program_id = $program_id
      """.query(int8.map(_ >= 1))

    val DeleteProposal: Command[Program.Id] =
      sql"""
        DELETE FROM t_proposal WHERE c_program_id = $program_id
      """.command

    def updates(SET: ProposalPropertiesInput.Edit): Option[NonEmptyList[AppliedFragment]] = {
      val mainUpdates: List[AppliedFragment] =
        List(
          SET.category.foldPresent(sql"c_category = ${tag.opt}"),
          SET.callId.foldPresent(sql"c_cfp_id = ${cfp_id.opt}")
        ).flatten

      val geminiUpdates: List[AppliedFragment] =
        SET.gemini.toList.flatMap { call =>
          // reset consider_for_band_3 for classical proposals.
          val considerForBand3Update =
            if call.scienceSubtype === ScienceSubtype.Classical then
              sql"c_consider_for_band_3 = ${consider_for_band_3}"(ConsiderForBand3.Unset).some
            else
              call.considerForBand3.map(sql"c_consider_for_band_3 = ${consider_for_band_3}")

          // A Gemini proposal sits at Gemini and has no Subaru proposal type.
          sql"c_observatory = ${observatory}"(Observatory.Gemini) ::
          sql"c_subaru_proposal_type = ${subaru_proposal_type.opt}"(none) ::
          sql"c_science_subtype = $science_subtype"(call.scienceSubtype) ::
          List(
            call.tooActivation.map(sql"c_too_activation = ${too_activation}"),
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

      // An external (Keck/Subaru) proposal carries an observatory (and, for
      // Subaru, a call type) and clears all Gemini-specific properties.  A Subaru
      // edit that omits the call type leaves the existing one unchanged; a Keck
      // proposal clears it.
      val externalUpdates: List[AppliedFragment] =
        if SET.keck.isEmpty && SET.subaru.isEmpty then Nil
        else {
          val (obs, subaruType): (Observatory, Nullable[SubaruCallForProposalsType]) =
            SET.subaru match
              case Some(s) => (Observatory.Subaru, Nullable.orAbsent(s.callType))
              case None    => (Observatory.Keck, Nullable.Null)
          List(
            sql"c_observatory = ${observatory}"(obs).some,
            subaruType.foldPresent(sql"c_subaru_proposal_type = ${subaru_proposal_type.opt}"),
            sql"c_science_subtype = ${science_subtype.opt}"(none).some,
            sql"c_too_activation = ${too_activation}"(ToOActivation.None).some,
            sql"c_min_percent = ${int_percent}"(IntPercent.unsafeFrom(0)).some,
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
          c_exchange_partner
        ) SELECT
          ${program_id},
          ${cfp_id.opt},
          ${tag.opt},
          ${observatory},
          ${science_subtype},
          ${too_activation},
          ${int_percent},
          ${int_percent.opt},
          ${time_span.opt},
          ${program_user_id.opt},
          ${program_user_id.opt},
          ${bool},
          ${bool},
          ${bool},
          ${consider_for_band_3},
          ${exchange_partner.opt}
      """.apply(
        pid,
        c.callId,
        c.category,
        Observatory.Gemini,
        g.scienceSubtype,
        g.tooActivation,
        g.minPercentTime,
        g.minPercentTotal,
        g.totalTime,
        g.reviewerId,
        g.mentorId,
        g.aeonMultiFacility,
        g.jwstSynergy,
        g.usLongTerm,
        g.considerForBand3,
        g.exchangePartner
      )

    // An external (exchange) proposal has no science subtype; it carries an
    // observatory and (for Subaru) a Subaru proposal type instead.  The
    // Gemini-specific columns take their table defaults; c_min_percent has no
    // default and must be 0 for an external proposal.
    private def insertExternalProposal(
      pid: Program.Id,
      c:   ProposalPropertiesInput.Create
    ): AppliedFragment =
      sql"""
        INSERT INTO t_proposal (
          c_program_id,
          c_cfp_id,
          c_category,
          c_observatory,
          c_subaru_proposal_type,
          c_min_percent
        ) SELECT
          ${program_id},
          ${cfp_id.opt},
          ${tag.opt},
          ${observatory},
          ${subaru_proposal_type.opt},
          ${int_percent}
      """.apply(
        pid,
        c.callId,
        c.category,
        c.observatory,
        c.subaruProposalType,
        IntPercent.unsafeFrom(0)
      )

    // The science subtype, observatory and Subaru proposal type are mirrored
    // directly from the proposal (so they may be cleared), while the semester and
    // proprietary period (which come from the CfP) are left unchanged when absent.
    val UpdateProgram: Command[(Program.Id, Option[ScienceSubtype], Observatory, Option[SubaruCallForProposalsType], Option[Semester], Option[NonNegInt])] =
      sql"""
        UPDATE t_program
           SET c_science_subtype      = ${science_subtype.opt},
               c_observatory          = ${observatory},
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
          prog.c_name,
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
          prog.c_goa_proprietary,
          LOCALTIMESTAMP,
          COALESCE(
            cfp_pi.c_deadline,
            (SELECT cfp.c_gemini_non_partner_deadline
             WHERE pi.c_partner_link = 'has_non_partner'),
            -- An exchange-partner request is not tied to any Gemini partner, so
            -- it uses the call's default submission deadline.
            (SELECT cfp.c_deadline_default
             WHERE prop.c_exchange_partner IS NOT NULL)
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
          prop.c_observatory,
          prop.c_subaru_proposal_type
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
        WHERE
          prog.c_program_id = $program_id
      """.apply(pid) |+|
      ProgramUserService.Statements.andWhereUserReadAccess(user, pid)

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
