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
import lucuma.core.util.Timestamp
import lucuma.odb.data.*
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.CreateProposalInput
import lucuma.odb.graphql.input.DeleteProposalInput
import lucuma.odb.graphql.input.ProposalPropertiesInput
import lucuma.odb.graphql.input.SetProposalStatusInput
import lucuma.odb.graphql.input.UpdateProposalInput
import lucuma.odb.syntax.resultT.*
import lucuma.odb.syntax.scienceSubtype.*
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.all.*
import skunk.data.Completion.Delete
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
   * The Call for Proposals submission deadline for the given call and partner.
   *
   * @param cid identifies the call for proposals
   * @param partner if None, the default submission deadline if any
   *
   * @return the deadline for this call and partner
   */
  def submissionDeadline(
    cid:     CallForProposals.Id,
    partner: Option[Tag]
  )(using Transaction[F]): F[Option[Timestamp]]

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
      s"The specified Call for Proposals $cid was not found.".invalidArg

    def creationFailed(pid: Program.Id): OdbError =
      s"Proposal creation failed because program $pid already has a proposal.".invalidArg

    def updateFailed(pid: Program.Id): OdbError =
      s"Proposal update failed because program $pid does not have a proposal.".invalidArg

    def mismatchedCfp(cid:  CallForProposals.Id, cfpType: CallForProposalsType, sub:  ScienceSubtype): OdbError =
      s"The Call for Proposals $cid is a ${cfpType.title} call and cannot be used with a ${sub.title} proposal.".invalidArg

    def invalidProposalStatus(ps: Tag): OdbError =
      s"Invalid proposal status: ${ps.value}".invalidArg

    def missingCfP(pid: Program.Id): OdbError =
      s"A Call for Proposals must be selected for $pid before submitting a proposal.".invalidArg

    def missingSemester(pid: Program.Id): OdbError =
      s"Submitted proposal $pid must be associated with a semester.".invalidArg

    def missingScienceSubtype(pid: Program.Id): OdbError =
      s"Submitted proposal $pid must have a science subtype.".invalidArg

    def missingOrInvalidSplits(pid: Program.Id, subtype: ScienceSubtype): OdbError =
      subtype match {
        case ScienceSubtype.FastTurnaround =>
          s"Submitted proposal $pid of type ${subtype.title} must specify the piAffiliation.".invalidArg
        case _ =>
          s"Submitted proposal $pid of type ${subtype.title} must specify partner time percentages which sum to 100%.".invalidArg
      }

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

      case class CfpProperties(
        cid:       CallForProposals.Id,
        callType:  CallForProposalsType,
        semester:  Semester
      ) {
        def validateSubtype(sub: ScienceSubtype): Result[Unit] =
          mismatchedCfp(cid, callType, sub)
            .asFailure
            .unlessA(sub.isCompatibleWith(callType))
      }

      object CfpProperties:
        val codec: Codec[CfpProperties] =
          (cfp_id *: cfp_type *: semester).to[CfpProperties]

        def lookup(cid: CallForProposals.Id)(using Transaction[F]): F[Result[CfpProperties]] =
          callForProposalsService
            .typeAndSemesterOf(cid)
            .map { o =>
              Result.fromOption(
                o.map(CfpProperties(cid, _, _)),
                cfpNotFound(cid).asProblem
              )
            }

      case class ProposalContext(
        statusTag:      Tag,
        hasProposal:    Boolean,
        semester:       Option[Semester],
        scienceSubtype: Option[ScienceSubtype],
        splitsSum:      Long,
        cfp:            Option[CfpProperties]
      ) {
        val status: Result[enumsVal.ProposalStatus] =
          statusTag.toProposalStatus

        def validateSubmission(
          pid: Program.Id,
          newStatus: enumsVal.ProposalStatus
        ): Result[Unit] =
          (
            missingProposal(pid).asFailure.unlessA(hasProposal),
            missingCfP(pid).asFailure.unlessA(cfp.isDefined),
            missingSemester(pid).asFailure.unlessA(semester.isDefined),
            missingScienceSubtype(pid).asFailure.unlessA(scienceSubtype.isDefined),
            scienceSubtype.fold(().success) { s =>
              missingOrInvalidSplits(pid, s).asFailure.whenA(
                splitsSum =!= 100 &&
                ((s === ScienceSubtype.Classical)      ||
                 (s === ScienceSubtype.FastTurnaround) ||
                 (s === ScienceSubtype.Queue))
              )
            }
          ).tupled.unlessA(newStatus === enumsVal.ProposalStatus.NotSubmitted)

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

        def edit(set: ProposalPropertiesInput.Edit)(using Transaction[F]): F[Result[ProposalContext]] = {
          val eCfp = set.callId.fold(
            ResultT.pure(none[CfpProperties]),                  // delete
            ResultT.pure(cfp),                                  // don't change
            id => ResultT(CfpProperties.lookup(id)).map(_.some) // update if possible
          )

          val eSum = set.typeʹ.fold(splitsSum) { t =>
            t.partnerSplits.fold(0.toLong, splitsSum, m => m.values.map(_.value.toLong).sum)
          }

          (for {
            c <- eCfp
            s <- eCfp.map(_.map(_.semester))
            t  = set.typeʹ.fold(scienceSubtype)(_.scienceSubtype.some)
          } yield copy(semester = s, scienceSubtype = t, splitsSum = eSum, cfp = c)).value
        }
      }

      object ProposalContext {
        val codec: Codec[ProposalContext] =
          (tag *: bool *: semester.opt *: science_subtype.opt *: int8 *: CfpProperties.codec.opt).to[ProposalContext]

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
          input.SET.callId.traverse(cid => ResultT(CfpProperties.lookup(cid)))

        // Make sure the indicated CfP is compatible with the inputs.
        def checkCfpCompatibility(o: Option[CfpProperties]): ResultT[F, Unit] =
          ResultT.fromResult(o.fold(Result.unit)(_.validateSubtype(input.SET.typeʹ.scienceSubtype)))

        // Update the program's science subtype and/or semester to match inputs.
        def updateProgram(p: ProposalContext, c: Option[CfpProperties]): ResultT[F, Unit] =
          ResultT.liftF(p.updateProgram(input.programId, input.SET.typeʹ.scienceSubtype.some, c.map(_.semester)))

        val insert: ResultT[F, Unit] =
          val af = Statements.insertProposal(input.programId, input.SET)
          val create = session.prepareR(af.fragment.command).use(_.execute(af.argument).void)
          ResultT(create.map(_.success).recover {
            case SqlState.UniqueViolation(e) =>
              error.creationFailed(input.programId).asFailure
          })

        val insertSplits: ResultT[F, Unit] =
          ResultT.liftF(
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
          ResultT.liftF(before.updateProgram(pid, after.scienceSubtype, after.semester))

        def handleTypeChange(pid: Program.Id, before: ProposalContext): ProposalPropertiesInput.Edit =
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
          })

        def updateSplits(pid: Program.Id, set: ProposalPropertiesInput.Edit): ResultT[F, Unit] =
          ResultT.liftF(Nullable.orAbsent(set.typeʹ).flatMap(_.partnerSplits).foldPresent( splits =>
            partnerSplitsService.updateSplits(splits.getOrElse(Map.empty), pid)
          ).sequence.void)

        (for {
          pid    <- ResultT(programService.resolvePid(input.programId, input.proposalReference, input.programReference))
          before <- ResultT(ProposalContext.lookup(pid))
          after  <- ResultT(before.edit(input.SET))
          _      <- checkCfpCompatibility(after)
          _      <- checkUserAccess(pid, after)
          _      <- ResultT.fromResult(after.status.flatMap(s => after.validateSubmission(pid, s)))
          _      <- ResultT.liftF(deferConstraints)
          set     = handleTypeChange(pid, before)
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

      override def submissionDeadline(
        cid:     CallForProposals.Id,
        partner: Option[Tag]
      )(using Transaction[F]): F[Option[Timestamp]] =
        session.unique(Statements.SelectSubmissionDeadline)(cid, partner)

      override def setProposalStatus(
        input: SetProposalStatusInput
      )(using Transaction[F], Services.PiAccess): F[Result[Program.Id]] = {

        def validate(
          pid: Program.Id,
          ctx: ProposalContext,
          oldStatus: enumsVal.ProposalStatus,
          newStatus: enumsVal.ProposalStatus
        ): Result[Unit] =
          for {
            _ <- notAuthorizedNew(pid, user, Tag(newStatus.tag)).asFailure.unlessA(newStatus.userCanChangeStatus)
            _ <- notAuthorizedOld(pid, user, ctx.statusTag).asFailure.unlessA(oldStatus.userCanChangeStatus)
            _ <- ctx.validateSubmission(pid, newStatus)
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
          SET.abstractʹ.foldPresent(sql"c_abstract = ${text_nonempty.opt}"),
          SET.category.foldPresent(sql"c_category = ${tag.opt}"),
          SET.title.foldPresent(sql"c_title = ${text_nonempty.opt}"),
          SET.callId.foldPresent(sql"c_cfp_id = ${cfp_id.opt}")
        ).flatten

      val callUpdates: List[AppliedFragment] =
        SET.typeʹ.toList.flatMap { call =>
          sql"c_science_subtype = $science_subtype"(call.scienceSubtype) ::
          List(
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
          c_cfp_id,
          c_title,
          c_abstract,
          c_category,
          c_science_subtype,
          c_too_activation,
          c_min_percent,
          c_min_percent_total,
          c_total_time
        ) SELECT
          ${program_id},
          ${cfp_id.opt},
          ${text_nonempty.opt},
          ${text_nonempty.opt},
          ${tag.opt},
          ${science_subtype},
          ${too_activation},
          ${int_percent},
          ${int_percent.opt},
          ${time_span.opt}
      """.apply(
        pid,
        c.callId,
        c.title,
        c.abstractʹ,
        c.category,
        c.typeʹ.scienceSubtype,
        c.typeʹ.tooActivation,
        c.typeʹ.minPercentTime,
        c.typeʹ.minPercentTotal,
        c.typeʹ.totalTime
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
          prog.c_semester,
          prog.c_science_subtype,
          COALESCE((SELECT SUM(c_percent) FROM t_partner_split WHERE c_program_id = prog.c_program_id), 0) AS c_splits_sum,
          cfp.c_cfp_id,
          cfp.c_type,
          cfp.c_semester
        FROM t_program prog
        LEFT JOIN t_proposal prop
          ON prog.c_program_id = prop.c_program_id
        LEFT JOIN t_cfp cfp
          ON prop.c_cfp_id = cfp.c_cfp_id
        WHERE
          prog.c_program_id = $program_id AND
          (prop.c_cfp_id IS NULL OR cfp.c_existence = 'present'::e_existence)
      """.apply(pid) |+|
      ProgramService.Statements.andWhereUserAccess(user, pid)

    def updateProposalStatus(user: User, pid: Program.Id, status: Tag): AppliedFragment =
      sql"""
        UPDATE t_program
        SET c_proposal_status = $tag
        WHERE c_program_id = $program_id
      """.apply(status, pid) |+|
      ProgramService.Statements.andWhereUserAccess(user, pid)

//    val SelectDefaultSubmissionDeadline: Query[Program.Id, Timestamp] =
//      sql"""SELECT c_deadline FROM t_cfp WHERE c_cfp_id = $program_id"""
//        .query(core_timestamp)

    val SelectSubmissionDeadline: Query[(CallForProposals.Id, Option[Tag]), Option[Timestamp]] =
      sql"""
        SELECT
          CASE
            WHEN c.c_type = 'directors_time' OR c.c_type = 'poor_weather' THEN c.c_deadline
            ELSE (SELECT p.c_deadline FROM t_cfp_partner p WHERE p.c_cfp_id = c.c_cfp_id AND p.c_partner = ${tag.opt})
          END AS c_deadline
        FROM
          t_cfp c
        WHERE
          c.c_cfp_id = $cfp_id
      """.query(core_timestamp.opt).contramap { case (t, p) => (p, t) }

//    val SelectSubmissionDeadlineForPartner: Query[(Program.Id, Tag), Timestamp] =
//      sql"""
//        SELECT
//          CASE
//            WHEN EXISTS(SELECT 1 FROM t_cfp_partner p WHERE p.c_cfp_id = c.c_cfp_id) THEN
//              (SELECT p.c_deadline FROM t_cfp_partner p WHERE p.c_cfp_id = c.c_cfp_id AND p.c_partner = $tag)
//            ELSE c.c_deadline
//          END AS c_deadline
//        FROM
//          t_cfp c
//        WHERE
//          c.c_cfp_id = $program_id
//      """.query(core_timestamp).contramap { case (t, p) => (p, t) }

  }
}