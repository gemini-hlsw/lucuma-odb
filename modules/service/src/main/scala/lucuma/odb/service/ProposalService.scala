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
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.ToOActivation
import lucuma.core.model.Access
import lucuma.core.model.CallForProposals
import lucuma.core.model.Program
import lucuma.core.model.ProposalReference
import lucuma.core.model.Semester
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.data.*
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.CreateProposalInput
import lucuma.odb.graphql.input.DeleteProposalInput
import lucuma.odb.graphql.input.ProposalPropertiesInput
import lucuma.odb.graphql.input.SetProposalStatusInput
import lucuma.odb.graphql.input.UpdateProposalInput
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.syntax.scienceSubtype.*
import lucuma.odb.util.Codecs.*
import org.http4s.Uri
import org.http4s.client.Client
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

    def missingPartners(pid: Program.Id, partners: Set[Tag] = Set.empty): OdbError =
      partners.toList.map(_.value.toUpperCase).sorted match
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

    def notAuthorizedNew(pid: Program.Id, user: User, ps: Tag): OdbError =
      s"User ${user.id} not authorized to set proposal status to ${ps.value.toUpperCase} in program $pid.".noAuth(user.id)

    def notAuthorizedOld(pid: Program.Id, user: User, ps: Tag): OdbError =
      s"User ${user.id} not authorized to change proposal status from ${ps.value.toUpperCase} in program $pid.".noAuth(user.id)

    def undefinedObservations(pid: Program.Id): OdbError =
      s"Submitted proposal $pid contains undefined observations.".invalidArg

    def missingPiEmailAddress(pid: Program.Id): OdbError =
      s"Missing email address for PI in program $pid".invalidArg

    def invalidPiEmailAddress(email: String, pid: Program.Id): OdbError =
      s"Invalid email address \"$email\" for PI in program $pid".invalidArg

  }

  /** Construct a `ProposalService` using the specified `Session`. */
  def instantiate[F[_]: Concurrent](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): ProposalService[F] =
    new ProposalService[F] {

      import error.*

      // A stable identifier (ie. a `val`) is needed for the enums.
      val enumsVal = enums

      extension (tag: Tag)
        def toProposalStatus: Result[enumsVal.ProposalStatus] =
          Enumerated[enumsVal.ProposalStatus]
            .fromTag(tag.value)
            .fold(invalidProposalStatus(tag).asFailure)(Result.apply)

      extension (ps: enumsVal.ProposalStatus)
        def userCanChangeStatus: Boolean =
          ps <= enumsVal.ProposalStatus.Submitted ||
          user.role.access >= Access.Ngo

        def userCanEditProposal: Boolean =
          ps < enumsVal.ProposalStatus.Submitted ||
          user.role.access >= Access.Ngo

      def lookupProperties(cid: CallForProposals.Id)(using Transaction[F]): F[Result[CfpProperties]] =
        callForProposalsService
          .selectProperties(cid)
          .map(o => Result.fromOption(o, cfpNotFound(cid).asProblem))

      case class ProposalContext(
        statusTag:         Tag,
        hasProposal:       Boolean,
        piEmailStr:        Option[NonEmptyString],
        title:             Option[NonEmptyString],
        reference:         Option[ProposalReference],
        semester:          Option[Semester],
        scienceSubtype:    Option[ScienceSubtype],
        splitsSum:         Long,
        availablePartners: Set[Tag],
        requestedPartners: Set[Tag],
        proprietary:       NonNegInt,
        currentTime:       Timestamp,
        deadline:          Option[Timestamp],
        cfpTitle:          Option[NonEmptyString],
        cfp:               Option[CfpProperties]
      ) {
        val status: Result[enumsVal.ProposalStatus] =
          statusTag.toProposalStatus

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
          newStatus: enumsVal.ProposalStatus
        ): Result[Unit] =
          val unmatchedPartners = requestedPartners -- availablePartners
          (
            missingProposal(pid).asFailure.unlessA(hasProposal),
            missingCfP(pid).asFailure.unlessA(cfp.isDefined),
            missingSemester(pid).asFailure.unlessA(semester.isDefined),
            missingScienceSubtype(pid).asFailure.unlessA(scienceSubtype.isDefined),
            scienceSubtype.fold(().success) { s =>
              missingOrInvalidSplits(pid, s).asFailure.whenA(
                splitsSum =!= 100 &&
                ((s === ScienceSubtype.Classical) ||
                 (s === ScienceSubtype.Queue))
              )
            },
            missingPartners(pid, unmatchedPartners).asFailure.unlessA(unmatchedPartners.isEmpty),
            validatePiEmailAddress(pid)
          ).tupled.unlessA(newStatus === enumsVal.ProposalStatus.NotSubmitted)

        def validateDeadline(
          pid: Program.Id,
          newStatus: enumsVal.ProposalStatus
        ): Result[Unit] =
          (
            for
              _ <- missingDeadline(pid).asFailure.unlessA(deadline.isDefined)
              _ <- pastDeadline(pid).asFailure.whenA(isPastDeadline.exists(identity))
            yield ()
          ).whenA(newStatus === enumsVal.ProposalStatus.Submitted)

        def updateProgram(
          pid:            Program.Id,
          newType:        Option[ScienceSubtype],
          newSemester:    Option[Semester],
          newProprietary: Option[NonNegInt]
        ): F[Unit] =
          session
            .execute(Statements.UpdateProgram)(pid, newType, newSemester, newProprietary)
            .whenA(
              newType.exists(t => scienceSubtype.forall(_ =!= t)) ||
              newSemester.exists(s => semester.forall(_ =!= s)) ||
              newProprietary.exists(p => proprietary =!= p)
            )

        def edit(set: ProposalPropertiesInput.Edit)(using Transaction[F]): F[Result[ProposalContext]] = {
          val eCfp = set.callId.fold(
            ResultT.pure(none[CfpProperties]),              // delete
            ResultT.pure(cfp),                              // don't change
            id => ResultT(lookupProperties(id)).map(_.some) // update if possible
          )

          val eSum = set.typeʹ.fold(splitsSum) { t =>
            t.partnerSplits.fold(0.toLong, splitsSum, m => m.values.map(_.value.toLong).sum)
          }

          (for {
            c <- eCfp
            s <- eCfp.map(_.map(_.semester))
            t  = set.typeʹ.fold(scienceSubtype)(_.scienceSubtype.some)
            p <- eCfp.map(_.map(_.proprietary).getOrElse(proprietary))
          } yield copy(semester = s, scienceSubtype = t, splitsSum = eSum, proprietary = p, cfp = c)).value
        }

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
            emailService(emailConfig, httpClient)
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
          newStatus: enumsVal.ProposalStatus
         )(using Transaction[F]): F[Result[Unit]] =
          // There might be other emails in the future
          if newStatus === enumsVal.ProposalStatus.Submitted then sendSubmissionEmail(pid)
          else Result.unit.pure

      }

      object ProposalContext {
        val parts: Decoder[Set[Tag]] =
          _tag.map(_.toList.toSet)

        val coNames: Decoder[List[Option[NonEmptyString]]] =
          _text.map(_.toList.map(n => NonEmptyString.from(n).toOption))

        val codec: Decoder[ProposalContext] =
          (tag *: bool *: varchar_nonempty.opt *: text_nonempty.opt *: proposal_reference.opt *: semester.opt *: science_subtype.opt *: int8 *: parts *: parts *: int4_nonneg *: core_timestamp *: core_timestamp.opt *: text_nonempty.opt *: CallForProposalsService.Statements.cfp_properties.opt).to[ProposalContext]

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

        // Make sure the indicated CfP is compatible with the inputs.
        def checkCfpCompatibility(o: Option[CfpProperties]): ResultT[F, Unit] =
          ResultT.fromResult(o.fold(Result.unit)(_.validateSubtype(input.SET.typeʹ.scienceSubtype)))

        // Update the program's science subtype and/or semester to match inputs.
        def updateProgram(p: ProposalContext, c: Option[CfpProperties]): ResultT[F, Unit] =
          ResultT.liftF(p.updateProgram(input.programId, input.SET.typeʹ.scienceSubtype.some, c.map(_.semester), c.map(_.proprietary)))

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
              partnerSplitsService.insertSplits(input.SET.typeʹ.partnerSplits, input.programId)
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

        // Make sure the indicated CfP is compatible with the inputs.
        def checkCfpCompatibility(p: ProposalContext): ResultT[F, Unit] =
          ResultT.fromResult((p.cfp, p.scienceSubtype).tupled.fold(Result.unit) { (c, s) => c.validateSubtype(s) })

        def checkUserAccess(pid: Program.Id, p: ProposalContext): ResultT[F, Unit] =
          ResultT.fromResult(
            p.status.flatMap { s =>
              cannotEditSubmittedProposal(pid, user).asFailure.unlessA(s.userCanEditProposal)
            }
          )

        // Update the program's science subtype and/or semester to match inputs.
        def updateProgram(pid: Program.Id, before: ProposalContext, after: ProposalContext): ResultT[F, Unit] =
          ResultT.liftF(before.updateProgram(pid, after.scienceSubtype, after.semester, after.proprietary.some))

        def handleTypeChange(before: ProposalContext): ProposalPropertiesInput.Edit =
          input.SET.typeʹ.filterNot(c => before.scienceSubtype.exists(_ === c.scienceSubtype)).fold(input.SET) { call =>
            input.SET.copy(typeʹ = call.asCreate.asEdit.some)
          }

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
          ResultT.liftF(Nullable.orAbsent(set.typeʹ).flatMap(_.partnerSplits).foldPresent( splits =>
            Services.asSuperUser:
              partnerSplitsService.updateSplits(splits.getOrElse(Map.empty), pid)
          ).sequence.void)

        (for {
          pid    <- ResultT(programService(emailConfig, httpClient).resolvePid(input.programId, input.proposalReference, input.programReference))
          before <- ResultT(ProposalContext.lookup(pid))
          after  <- ResultT(before.edit(input.SET))
          _      <- checkCfpCompatibility(after)
          _      <- checkUserAccess(pid, after)
          _      <- ResultT.fromResult(after.status.flatMap(s => after.validateSubmission(pid, s)))
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
          oldStatus: enumsVal.ProposalStatus,
          newStatus: enumsVal.ProposalStatus
        ): Result[Unit] =
          for {
            _ <- undefinedObservations(pid).asFailure.whenA(states.contains(ObservationWorkflowState.Undefined))
            _ <- notAuthorizedNew(pid, user, Tag(newStatus.tag)).asFailure.unlessA(newStatus.userCanChangeStatus)
            _ <- notAuthorizedOld(pid, user, ctx.statusTag).asFailure.unlessA(oldStatus.userCanChangeStatus)
            _ <- ctx.validateSubmission(pid, newStatus)
            _ <- ctx.validateDeadline(pid, newStatus)
          } yield ()

        def update(pid: Program.Id, tag: Tag): F[Unit] =
          val af = Statements.updateProposalStatus(user, pid, tag)
          session.prepareR(af.fragment.command).use(_.execute(af.argument)).void

        ResultT(programService(emailConfig, httpClient).resolvePid(input.programId, input.proposalReference, input.programReference))
          .flatMap: pid =>
            ResultT(Services.asSuperUser(observationWorkflowService(httpClient).getWorkflows(pid, commitHash, itcClient, ptc))).flatMap: wfs =>
              val states = wfs.values.map(_.state).toSet
              ResultT:
                services.transactionally:
                  val go2 =
                    for
                      info      <- ResultT(ProposalContext.lookup(pid))
                      oldStatus <- ResultT.fromResult(info.status) // This 'should' be succesful, since it is from the DB
                      newStatus <- ResultT.fromResult(input.status.toProposalStatus)
                      _         <- ResultT.fromResult(validate(pid, info, states, oldStatus, newStatus))
                      _         <- ResultT.liftF(update(pid, input.status))
                      _         <- ResultT(configurationService.canonicalizeAll(pid)).whenA(oldStatus === enumsVal.ProposalStatus.NotSubmitted && newStatus === enumsVal.ProposalStatus.Submitted)
                      _         <- ResultT(configurationService.deleteAll(pid)).whenA(oldStatus === enumsVal.ProposalStatus.Submitted && newStatus === enumsVal.ProposalStatus.NotSubmitted)
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

      val callUpdates: List[AppliedFragment] =
        SET.typeʹ.toList.flatMap { call =>
          sql"c_science_subtype = $science_subtype"(call.scienceSubtype) ::
          List(
            call.tooActivation.map(sql"c_too_activation = ${too_activation}"),
            call.minPercentTime.map(sql"c_min_percent = ${int_percent}"),
            call.minPercentTotal.foldPresent(sql"c_min_percent_total = ${int_percent.opt}"),
            call.totalTime.foldPresent(sql"c_total_time = ${time_span.opt}"),
            call.reviewerId.foldPresent(sql"c_reviewer_id = ${program_user_id.opt}"),
            call.mentorId.foldPresent(sql"c_mentor_id = ${program_user_id.opt}")
          ).flatten
        }

      NonEmptyList.fromList(mainUpdates ++ callUpdates)
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
      sql"""
        INSERT INTO t_proposal (
          c_program_id,
          c_cfp_id,
          c_category,
          c_science_subtype,
          c_too_activation,
          c_min_percent,
          c_min_percent_total,
          c_total_time,
          c_reviewer_id,
          c_mentor_id
        ) SELECT
          ${program_id},
          ${cfp_id.opt},
          ${tag.opt},
          ${science_subtype},
          ${too_activation},
          ${int_percent},
          ${int_percent.opt},
          ${time_span.opt},
          ${program_user_id.opt},
          ${program_user_id.opt}
      """.apply(
        pid,
        c.callId,
        c.category,
        c.typeʹ.scienceSubtype,
        c.typeʹ.tooActivation,
        c.typeʹ.minPercentTime,
        c.typeʹ.minPercentTotal,
        c.typeʹ.totalTime,
        c.typeʹ.reviewerId,
        c.typeʹ.mentorId
      )

    val UpdateProgram: Command[(Program.Id, Option[ScienceSubtype], Option[Semester], Option[NonNegInt])] =
      sql"""
        UPDATE t_program
           SET c_science_subtype = CASE
                                     WHEN ${science_subtype.opt} IS NULL THEN c_science_subtype
                                     ELSE ${science_subtype.opt}
                                   END,
               c_semester        = CASE
                                     WHEN ${semester.opt} IS NULL THEN c_semester
                                     ELSE ${semester.opt}
                                   END,
               c_goa_proprietary = CASE
                                     WHEN ${int4_nonneg.opt} is NULL THEN c_goa_proprietary
                                     ELSE ${int4_nonneg.opt}
                                   END
         WHERE c_program_id = $program_id
      """.command.contramap { case (p, t, s, r) => (t, t, s, s, r, r, p) }

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
                   ELSE c_partner
                 END
               )
             FROM t_program_user
             WHERE c_program_id = prog.c_program_id
               AND (c_partner IS NOT NULL OR c_partner_link = 'has_non_partner')
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
            (SELECT cfp.c_non_partner_deadline
             WHERE pi.c_partner_link = 'has_non_partner')
          ) AS c_deadline,
          cfp.c_title,
          cfp.c_cfp_id,
          cfp.c_type,
          cfp.c_semester,
          cfp.c_proprietary
        FROM t_program prog
        LEFT JOIN t_proposal prop
          ON prog.c_program_id = prop.c_program_id
        LEFT JOIN v_cfp cfp
          ON prop.c_cfp_id = cfp.c_cfp_id
        LEFT JOIN v_program_user pi
          ON prog.c_program_id = pi.c_program_id
          AND pi.c_role = 'pi'
        LEFT JOIN v_cfp_partner cfp_pi
          ON cfp.c_cfp_id = cfp_pi.c_cfp_id
          AND cfp_pi.c_partner = pi.c_partner
        WHERE
          prog.c_program_id = $program_id
      """.apply(pid) |+|
      ProgramUserService.Statements.andWhereUserReadAccess(user, pid)

    def updateProposalStatus(user: User, pid: Program.Id, status: Tag): AppliedFragment =
      sql"""
        UPDATE t_program
        SET c_proposal_status = $tag
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
