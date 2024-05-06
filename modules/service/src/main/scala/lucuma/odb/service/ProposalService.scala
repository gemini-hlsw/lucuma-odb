// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.ToOActivation
import lucuma.core.model.Access
import lucuma.core.model.CallForProposals
import lucuma.core.model.IntPercent
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.odb.data.*
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.CreateProposalInput
import lucuma.odb.graphql.input.ProposalPropertiesInput
import lucuma.odb.graphql.input.SetProposalStatusInput
import lucuma.odb.graphql.input.UpdateProposalInput
import lucuma.odb.syntax.resultT.*
import lucuma.odb.syntax.scienceSubtype.*
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.all.*
import skunk.data.Completion.Update
import skunk.syntax.all.*

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
   * Set the proposal status associated with the program specified in the `input`.
   */
  def setProposalStatus(
    input: SetProposalStatusInput
  )(using Transaction[F], Services.PiAccess): F[Result[Program.Id]]

}

object ProposalService {

  object error {
    extension (s: String)
      def invalidArg: OdbError           = OdbError.InvalidArgument(s.some)
      def noAuth(uid: User.Id): OdbError = OdbError.NotAuthorized(uid, s.some)

    def cfpNotFound(cid: CallForProposals.Id): OdbError =
      s"The specified Call for Proposals ($cid) was not found.".invalidArg

    def creationFailed(pid: Program.Id): OdbError =
      s"Proposal creation failed because program $pid already has a proposal.".invalidArg

    def updateFailed(pid: Program.Id): OdbError =
      s"Proposal update failed because program $pid does not have a proposal.".invalidArg

    def mismatchedCfp(cid:  CallForProposals.Id, cfpType: CallForProposalsType, sub:  ScienceSubtype): OdbError =
      s"The indicated Call for Proposals ($cid) is a ${cfpType.title} call and cannot be used with a ${sub.title} proposal.".invalidArg

    def invalidProposalStatus(ps: Tag): OdbError =
      s"Invalid proposal status: ${ps.value}".invalidArg

    def missingCfP(pid: Program.Id): OdbError =
      s"A Call for Proposals must be selected for $pid before submitting a proposal.".invalidArg

    def missingSemester(pid: Program.Id): OdbError =
      s"Submitted program $pid must be associated with a semester.".invalidArg

    def missingScienceSubtype(pid: Program.Id): OdbError =
      s"Submitted program $pid must have a science subtype.".invalidArg

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
  }

  /** Construct a `ProposalService` using the specified `Session`. */
  def instantiate[F[_]: Concurrent: Trace](using Services[F]): ProposalService[F] =
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

      case class ProposalContext(
        statusTag:      Tag,
        hasProposal:    Boolean,
        cfpId:          Option[CallForProposals.Id],
        semester:       Option[Semester],
        scienceSubtype: Option[ScienceSubtype]
      ) {
        val status: Result[enumsVal.ProposalStatus] =
          statusTag.toProposalStatus

        def updateProgram(
          pid:         Program.Id,
          newType:     Option[ScienceSubtype],
          newSemester: Option[Semester]
        ): F[Unit] =
          session
            .execute(Statements.UpdateProgram)(pid, newType, newSemester)
            .whenA(
              newType.exists(t => scienceSubtype.forall(_ =!= t)) ||
              newSemester.exists(s => semester.forall(_ =!= s))
            )
      }

      object ProposalContext {

        private val proposal_context: Codec[ProposalContext] =
          (tag *: bool *: cfp_id.opt *: semester.opt *: science_subtype.opt).to[ProposalContext]

        def lookup(pid: Program.Id): F[Result[ProposalContext]] =
          val af = Statements.selectProposalContext(user, pid)
          session.prepareR(
            af.fragment.query(program_type *: proposal_context)
          ).use { ps =>
            ps.option(af.argument).map {
              case Some((ProgramType.Science, pc)) => pc.success
              case Some((t, pc))                   => invalidProgramType(pid, t).asFailure
              case _                               => OdbError.InvalidProgram(pid).asFailure
            }
          }

      }

      case class CfpProperties(
        cid:      CallForProposals.Id,
        callType: CallForProposalsType,
        semester: Semester
      ) {
        def validateSubtype(sub: ScienceSubtype): Result[Unit] =
          mismatchedCfp(cid, callType, sub)
            .asFailure
            .unlessA(sub.isCompatibleWith(callType))
      }

      object CfpProperties:
        def lookup(cid: CallForProposals.Id)(using Transaction[F]): F[Result[CfpProperties]] =
          callForProposalsService
            .typeAndSemesterOf(cid)
            .map { o =>
              Result.fromOption(
                o.map(CfpProperties(cid, _, _)),
                cfpNotFound(cid).asProblem
              )
            }

      def createProposal(
        input: CreateProposalInput
      )(using Transaction[F], Services.PiAccess): F[Result[Program.Id]] = {

        def lookupCfpProperties: ResultT[F, Option[CfpProperties]] =
          input.SET.call.callId.traverse(cid => ResultT(CfpProperties.lookup(cid)))

        // Make sure the indicated CfP is compatible with the inputs.
        def checkCfpCompatibility(o: Option[CfpProperties]): ResultT[F, Unit] =
          ResultT.fromResult(o.fold(Result.unit)(_.validateSubtype(input.SET.call.scienceSubtype)))

        // Update the program's science subtype and/or semester to match inputs.
        def updateProgram(p: ProposalContext, c: Option[CfpProperties]): ResultT[F, Unit] =
          ResultT.liftF(p.updateProgram(input.programId, input.SET.call.scienceSubtype.some, c.map(_.semester)))

        val insert: ResultT[F, Unit] =
          val af = Statements.insertProposal(input.programId, input.SET)
          val create = session.prepareR(af.fragment.command).use(_.execute(af.argument).void)
          ResultT(create.map(_.success).recover {
            case SqlState.UniqueViolation(e) =>
              error.creationFailed(input.programId).asFailure
          })

        val insertSplits: ResultT[F, Unit] =
          ResultT.liftF(
            partnerSplitsService.insertSplits(input.SET.call.partnerSplits, input.programId)
          )

        (for {
          c <- lookupCfpProperties
          _ <- checkCfpCompatibility(c)
          p <- ResultT(ProposalContext.lookup(input.programId))
          _ <- updateProgram(p, c)
          _ <- insert
          _ <- insertSplits
        } yield input.programId).value

      }

      def updateProposal(
        input: UpdateProposalInput
      )(using Transaction[F], Services.PiAccess): F[Result[Program.Id]] = {

        val callId = input.SET.call.flatMap(_.callId.toOption)

        def lookupCfpProperties: ResultT[F, Option[CfpProperties]] =
          callId.traverse(cid => ResultT(CfpProperties.lookup(cid)))

        // Make sure the indicated CfP is compatible with the inputs.
        def checkCfpCompatibility(o: Option[CfpProperties]): ResultT[F, Unit] =
          ResultT.fromResult(
            (o, input.SET.call.map(_.scienceSubtype)).tupled.fold(Result.unit) { (c, s) =>
              c.validateSubtype(s)
            }
          )

        def checkUserAccess(pid: Program.Id, p: ProposalContext): ResultT[F, Unit] =
          ResultT.fromResult(
            p.status.flatMap { s =>
              cannotEditSubmittedProposal(pid, user).asFailure.unlessA(s.userCanEditProposal)
            }
          )

        // Update the program's science subtype and/or semester to match inputs.
        def updateProgram(pid: Program.Id, p: ProposalContext, c: Option[CfpProperties]): ResultT[F, Unit] =
          ResultT.liftF(p.updateProgram(pid, input.SET.call.map(_.scienceSubtype), c.map(_.semester)))

        def handleTypeChange(pid: Program.Id, oldType: Option[ScienceSubtype]): ResultT[F, ProposalPropertiesInput.Edit] =
          input.SET.call.filterNot(c => oldType.exists(_ === c.scienceSubtype)).fold(ResultT.pure(input.SET)) { call =>
            ResultT.fromResult(call.asCreate.map(create => input.SET.copy(call = create.asEdit.some)))
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
          })

        def updateSplits(pid: Program.Id, set: ProposalPropertiesInput.Edit): ResultT[F, Unit] =
          ResultT(set.call.map(_.partnerSplits).traverse_ { splits =>
            partnerSplitsService.updateSplits(splits.getOrElse(Map.empty), pid)
          }.map(_.success))

        (for {
          pid <- ResultT(programService.resolvePid(input.programId, input.proposalReference, input.programReference))
          c   <- lookupCfpProperties
          _   <- checkCfpCompatibility(c)
          p   <- ResultT(ProposalContext.lookup(pid))
          _   <- checkUserAccess(pid, p)
          _   <- updateProgram(pid, p, c)
          set <- handleTypeChange(pid, p.scienceSubtype)
          _   <- updateProposal(pid, set)
          _   <- updateSplits(pid, set)
        } yield pid).value
      }

      def setProposalStatus(
        input: SetProposalStatusInput
      )(using Transaction[F], Services.PiAccess): F[Result[Program.Id]] = {

        def validateProgramReference(pid: Program.Id, info: ProposalContext, newStatus: enumsVal.ProposalStatus): Result[Unit] =
          (
            missingCfP(pid).asFailure.unlessA(info.cfpId.isDefined),
            missingSemester(pid).asFailure.unlessA(info.semester.isDefined),
            missingScienceSubtype(pid).asFailure.unlessA(info.scienceSubtype.isDefined)
          ).tupled.unlessA(newStatus === enumsVal.ProposalStatus.NotSubmitted)

        def validate(
          pid: Program.Id,
          info: ProposalContext,
          oldStatus: enumsVal.ProposalStatus,
          newStatus: enumsVal.ProposalStatus
        ): Result[Unit] =
          for {
            _ <- missingProposal(pid).asFailure.unlessA(info.hasProposal)
            _ <- notAuthorizedNew(pid, user, Tag(newStatus.tag)).asFailure.unlessA(newStatus.userCanChangeStatus)
            _ <- notAuthorizedOld(pid, user, info.statusTag).asFailure.unlessA(oldStatus.userCanChangeStatus)
            _ <- validateProgramReference(pid, info, newStatus)
          } yield ()

        def update(pid: Program.Id, tag: Tag): F[Unit] =
          val af = Statements.updateProposalStatus(user, pid, tag)
          session.prepareR(af.fragment.command).use(_.execute(af.argument)).void

        (for {
          pid       <- ResultT(programService.resolvePid(input.programId, input.proposalReference, input.programReference))
          info      <- ResultT(ProposalContext.lookup(pid))
          oldStatus <- ResultT.fromResult(info.status) // This 'should' be succesful, since it is from the DB
          newStatus <- ResultT.fromResult(input.status.toProposalStatus)
          _         <- ResultT.fromResult(validate(pid, info, oldStatus, newStatus))
          _         <- ResultT.liftF(update(pid, input.status))
        } yield pid).value
      }
    }

  private object Statements {

    def updates(SET: ProposalPropertiesInput.Edit): Option[NonEmptyList[AppliedFragment]] = {
      val mainUpdates: List[AppliedFragment] =
        List(
          SET.abstrakt.foldPresent(sql"c_abstract = ${text_nonempty.opt}"),
          SET.category.foldPresent(sql"c_category = ${tag.opt}"),
          SET.title.foldPresent(sql"c_title = ${text_nonempty.opt}"),
        ).flatten

      val callUpdates: List[AppliedFragment] =
        SET.call.toList.flatMap { call =>
          List(
            call.callId.foldPresent(sql"c_cfp_id = ${cfp_id.opt}"),
            call.tooActivation.map(sql"c_too_activation = ${too_activation}"),
            call.minPercentTime.map(sql"c_min_percent = ${int_percent}"),
            call.minPercentTotal.foldPresent(sql"c_min_percent_total = ${int_percent.opt}"),
            call.totalTime.foldPresent(sql"c_total_time = ${time_span.opt}")
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
          c_title,
          c_abstract,
          c_category,
          c_science_subtype,
          c_cfp_id,
          c_too_activation,
          c_min_percent,
          c_min_percent_total,
          c_total_time
        ) SELECT
          ${program_id},
          ${text_nonempty.opt},
          ${text_nonempty.opt},
          ${tag.opt},
          ${science_subtype},
          ${cfp_id.opt},
          ${too_activation},
          ${int_percent},
          ${int_percent.opt},
          ${time_span.opt}
      """.apply(
        pid,
        c.title,
        c.abstrakt,
        c.category,
        c.call.scienceSubtype,
        c.call.callId,
        c.call.tooActivation,
        c.call.minPercentTime,
        c.call.minPercentTotal,
        c.call.totalTime
      )

    val UpdateProgram: Command[(Program.Id, Option[ScienceSubtype], Option[Semester])] =
      sql"""
        UPDATE t_program
           SET c_science_subtype = CASE
                                     WHEN ${science_subtype.opt} IS NULL THEN c_science_subtype
                                     ELSE ${science_subtype.opt}
                                   END,
               c_semester        = CASE
                                     WHEN ${semester.opt} IS NULL THEN c_semester
                                     ELSE ${semester.opt}
                                   END
         WHERE c_program_id = $program_id
      """.command.contramap { case (p, t, s) => (t, t, s, s, p) }

    def selectProposalContext(user: User, pid: Program.Id): AppliedFragment =
      sql"""
        SELECT 
          prog.c_program_type,
          prog.c_proposal_status,
          prop.c_program_id IS NOT NULL,
          prop.c_cfp_id,
          prog.c_semester,
          prog.c_science_subtype
        FROM t_program prog
        LEFT JOIN t_proposal prop
          ON prog.c_program_id = prop.c_program_id
        WHERE prog.c_program_id = $program_id
      """.apply(pid) |+|
      ProgramService.Statements.andWhereUserAccess(user, pid)

    def updateProposalStatus(user: User, pid: Program.Id, status: Tag): AppliedFragment =
      sql"""
        UPDATE t_program
        SET c_proposal_status = $tag
        WHERE c_program_id = $program_id
      """.apply(status, pid) |+|
      ProgramService.Statements.andWhereUserAccess(user, pid)

  }
}