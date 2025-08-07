// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.Ior
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ProgramType
import lucuma.core.model.Access
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProgramReference.Description
import lucuma.core.model.ProposalReference
import lucuma.core.model.Semester
import lucuma.core.model.ServiceUser
import lucuma.odb.Config
import lucuma.odb.data.*
import lucuma.odb.graphql.input.GoaPropertiesInput
import lucuma.odb.graphql.input.ProgramPropertiesInput
import lucuma.odb.graphql.input.ProgramReferencePropertiesInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import natchez.Trace
import org.http4s.client.Client
import skunk.*
import skunk.codec.all.*
import skunk.syntax.all.*

import java.time.LocalDate

import OdbErrorExtensions.*
import Services.Syntax.*

trait ProgramService[F[_]] {

  /**
   * Convenience method. Find the program id consistent with the provided ids (if any).
   */
  def resolvePid(
    pid:  Option[Program.Id],
    prop: Option[ProposalReference],
    prog: Option[ProgramReference]
  ): F[Result[Program.Id]]

  def setProgramReference(
    input: AccessControl.CheckedWithId[ProgramReferencePropertiesInput, Program.Id]
  )(using Transaction[F]): F[Result[(Program.Id, Option[ProgramReference])]]

  /**
   * Insert a new program, where the calling user becomes PI (unless it's a Service user, in which
   * case the PI is left empty.
   */
  def insertProgram(
    input: AccessControl.Checked[Option[ProgramPropertiesInput.Create]]
  )(using Transaction[F]): F[Result[Program.Id]]

  /**
   * Insert a new calibration program, PI is left empty.
   * N.B. Calibration programs are created as part of the database schema; this method is only called from tests.
   */
  def insertCalibrationProgram(SET: Option[ProgramPropertiesInput.Create], calibrationRole: CalibrationRole, description: Description)(using Transaction[F], SuperUserAccess): F[Program.Id]

  def updatePrograms(
    input: AccessControl.Checked[ProgramPropertiesInput.Edit]
  )(using Transaction[F]): F[Result[List[Program.Id]]]

}

object ProgramService {

  sealed trait UpdateProgramsError extends Product with Serializable {
    import UpdateProgramsError.*
    def message: String = this match
      case InvalidSemester(s)                            =>
        s"The maximum semester is capped at the current year +1${s.fold(".")(" ("+ _.format + " specified).")}"
      case DuplicateReference(r)                         =>
        s"""Program reference${r.fold("")(s => s" '$s'")} already exists."""

    def failure = this match
      case InvalidSemester(s) => OdbError.InvalidArgument(Some(message)).asFailure
      case DuplicateReference(r) => OdbError.InvalidArgument(Some(message)).asFailure

  }

  object UpdateProgramsError {
    case class InvalidSemester(s: Option[Semester]) extends UpdateProgramsError
    case class DuplicateReference(ref: Option[String]) extends UpdateProgramsError
  }

  /**
   * Construct a `ProgramService` using the specified `Session`, for the specified `User`. All
   * operations will be performed on behalf of `user`.
   */
  def instantiate[F[_]: Concurrent: Trace](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): ProgramService[F] =
    new ProgramService[F] {

      def selectPid(ref: Ior[ProposalReference, ProgramReference]): F[Option[Program.Id]] = {
        val af = Statements.selectPid(ref)
        session.prepareR(af.fragment.query(program_id)).use { ps =>
          ps.option(af.argument)
        }
      }

      override def resolvePid(
        pid:  Option[Program.Id],
        prop: Option[ProposalReference],
        prog: Option[ProgramReference]
      ): F[Result[Program.Id]] = {
        def notFound(ref: Ior[ProposalReference, ProgramReference]): String =
          ref.fold(
            r => s"Proposal '${r.label}' was not found.",
            r => s"Program '${r.label}' was not found.",
            (r0, r1) => s"Proposal '${r0.label}' and program '${r1.label}' were not found or do not correspond to the same program."
          )

        def notCorresponding(
          ref:      Ior[ProposalReference, ProgramReference],
          pid:      Program.Id,
          givenPid: Program.Id
        ): String =
          ref.fold(
            r => s"Proposal '${r.label}' (id $pid) does not correspond to the specified program id $givenPid.",
            r => s"Program '${r.label}' (id $pid) does not correspond to the specified program id $givenPid.",
            (r0, r1) => s"Proposal '${r0.label}' and program '${r1.label}' (id $pid) do not correspond to the specified program id $givenPid."
          )

        (pid, Ior.fromOptions(prop, prog)) match {
          case (None, None)    => OdbError.InvalidArgument("One of programId, programReference or proposalReference must be provided.".some).asFailureF
          case (Some(p), None) => p.success.pure[F]
          case (_, Some(r))    => selectPid(r).map { op =>
            op.fold(OdbError.InvalidArgument(notFound(r).some).asFailure) { selectedPid =>
              pid.fold(selectedPid.success) { givenPid =>
                OdbError.InvalidArgument(notCorresponding(r, selectedPid, givenPid).some)
                  .asFailure
                  .unlessA(selectedPid === givenPid)
                  .as(selectedPid)
              }
            }
          }
        }
      }

      private def setProgramReferenceImpl(id: Program.Id, input: ProgramReferencePropertiesInput): F[Result[Option[ProgramReference]]] =
        session
          .unique(Statements.SetProgramReference)(id, input)
          .map(_.success)
          .recover {
            case SqlState.CheckViolation(ex) if ex.getMessage.indexOf("d_semester_check") >= 0 =>
              UpdateProgramsError
                .InvalidSemester(input.semester)
                .failure
            case SqlState.UniqueViolation(ex) =>
              // See if we can parse out the duplicate reference string.
              val pat = """\(c_program_reference\)=\(([^)]+)\)""".r
              UpdateProgramsError
                .DuplicateReference(pat.findFirstMatchIn(ex.getMessage).map(_.group(1)))
                .failure
          }

      override def setProgramReference(input: AccessControl.CheckedWithId[ProgramReferencePropertiesInput, Program.Id])(using Transaction[F]): F[Result[(Program.Id, Option[ProgramReference])]] = 
        input.foldWithId(OdbError.InvalidArgument().asFailureF): (props, pid) =>
          def validateProposal(pid: Program.Id): F[Result[Unit]] =
            proposalService(emailConfig, httpClient).hasProposal(pid).map { hasProposal =>
              OdbError
                .InvalidProgram(pid, s"Cannot set the program reference for $pid to ${props.programType.abbreviation} until its proposal is removed.".some)
                .asFailure
                .whenA(hasProposal && props.programType != ProgramType.Science)
            }
          (for {
            _ <- ResultT(validateProposal(pid))
            r <- ResultT(setProgramReferenceImpl(pid, props).map(_.map((pid, _))))
          } yield r).value

      def validateActivePeriodUpdate[A](active: Option[A]): Result[Unit] =
        OdbError
          .NotAuthorized(user.id, s"Only staff members may change the program active period.".some)
          .asFailure
          .unlessA(
            user.role.access match
              case Access.Admin | Access.Service | Access.Staff => true
              case _                                            => active.isEmpty
          )

      def validateProprietaryPeriod(period: Option[NonNegInt]): Result[Unit] =
        OdbError
          .NotAuthorized(user.id, "Only staff may set the proprietary months.".some)
          .asFailure
          .unlessA(
            user.role.access match
              case Access.Admin | Access.Service | Access.Staff => true
              case _                                            => period.isEmpty
          )

      def insertProgram(
        input: AccessControl.Checked[Option[ProgramPropertiesInput.Create]]
      )(using Transaction[F]): F[Result[Program.Id]] =
        input.fold(OdbError.InvalidArgument().asFailureF): (SET, _) =>
          Trace[F].span("insertProgram") {
            val SETʹ = SET.getOrElse(ProgramPropertiesInput.Create.Default)

            val create =
              session
                .prepareR(Statements.InsertProgram)
                .use(_.unique(SETʹ))
                .flatTap: pid =>
                  user match
                    case ServiceUser(_, _) =>
                      Concurrent[F].unit
                    case nonServiceUser    =>
                      // Link the PI to the program.
                      programUserService.addAndLinkPi(pid).void
                .map(_.success)

            val proprietaryMonths =
              SETʹ.goa.proprietaryMonths.some.filter(_ =!= GoaPropertiesInput.DefaultProprietaryMonths)

            def setActivePeriod(p: Program.Id, a: Ior[LocalDate, LocalDate]): F[Result[Unit]] =
              val edit = ProgramPropertiesInput.Edit.Default.copy(active = a.some)
              updatePrograms(
                Services.asSuperUser:
                  // Irritating, we need to reign in the bare AppliedFragments because there's no 
                  // way to know what form they should take. Sometimes it's IN (frag), other
                  // times (as below) it's FROM (frag) and you need a SELECT in there.
                  AccessControl.unchecked(edit, sql"select $program_id".apply(p))
              ).map(_.void)

            (for {
              _ <- ResultT.fromResult(validateActivePeriodUpdate(SETʹ.active))
              _ <- ResultT.fromResult(validateProprietaryPeriod(proprietaryMonths))
              p <- ResultT(create)
              _ <- SETʹ.active.fold(ResultT.unit)(a => ResultT(setActivePeriod(p, a)))
            } yield p).value
          }


      def insertCalibrationProgram(SET: Option[ProgramPropertiesInput.Create], calibrationRole: CalibrationRole, description: Description)(using Transaction[F], SuperUserAccess): F[Program.Id] =
        Trace[F].span("insertCalibrationProgram") {
          val SETʹ = SET.getOrElse(ProgramPropertiesInput.Create.Default)

          session.prepareR(Statements.InsertCalibrationProgram).use(_.unique(SETʹ.name, calibrationRole, description.value))
        }

      override def updatePrograms(input: AccessControl.Checked[ProgramPropertiesInput.Edit])(using Transaction[F]): F[Result[List[Program.Id]]] = {
        input.fold(Result(Nil).pure[F]): (SET, where) =>

          // Create the temp table with the programs we're updating. We will join with this
          // several times later on in the transaction.
          val setup: F[Unit] =
            val af = Statements.createProgramUpdateTempTable(where)
            session.prepareR(af.fragment.command).use(_.execute(af.argument)).void

          // Update programs
          val updatePrograms: F[Result[List[Program.Id]]] =
            Statements.updatePrograms(SET).fold(Nil.success.pure[F]): af =>
              session.prepareR(af.fragment.query(program_id)).use: ps =>
                ps.stream(af.argument, 1024)
                  .compile
                  .toList
                  .map(_.success)
                  .recover {
                    case SqlState.CheckViolation(e) if e.constraintName.contains("active_dates_check") =>
                      OdbError.InvalidArgument("Requested update to the active period is invalid: activeStart must come before activeEnd".some).asFailure
                  }

          (for
            _   <- ResultT.liftF(setup)
            _   <- ResultT.fromResult(validateProprietaryPeriod(SET.goa.flatMap(_.proprietaryMonths)))
            _   <- ResultT.fromResult(validateProprietaryPeriod(SET.goa.flatMap(_.proprietaryMonths)))
            ids <- ResultT(updatePrograms)
          yield ids).value

      }

    }

  private object Statements {

    def selectPid(ref: Ior[ProposalReference, ProgramReference]): AppliedFragment =
      void"""
        SELECT
          c_program_id
        FROM
          t_program
        WHERE
      """ |+| ref.fold(
        sql"c_proposal_reference = $proposal_reference",
        sql"c_program_reference = $program_reference",
        (prop, prog) => sql"c_proposal_reference = $proposal_reference AND c_program_reference = $program_reference".apply(prop, prog)
      )

    val SetProgramReference: Query[(Program.Id, ProgramReferencePropertiesInput), Option[ProgramReference]] =
      sql"""
        UPDATE
          t_program
        SET
          c_program_type    = $program_type,
          c_library_desc    = ${text.opt},
          c_instrument      = ${instrument.opt},
          c_semester        = ${semester.opt},
          c_science_subtype = ${science_subtype.opt}
        WHERE
          c_program_id = $program_id
        RETURNING
          c_program_reference
      """.query(program_reference.opt)
         .contramap[(Program.Id, ProgramReferencePropertiesInput)] { (id, prpi) => (
           prpi.programType,
           prpi.description.map(_.value),
           prpi.instrument,
           prpi.semester,
           prpi.scienceSubtype,
           id
         )}

    def createProgramUpdateTempTable(whichProgramIds: AppliedFragment): AppliedFragment =
      void"""
        CREATE TEMPORARY TABLE t_program_update (
          c_program_id
        )
        ON COMMIT DROP
          AS SELECT
            which.pid
        FROM (""" |+| whichProgramIds |+| void""") AS which (pid)
        INNER JOIN t_program prog
          ON prog.c_program_id = which.pid
      """

    def updates(SET: ProgramPropertiesInput.Edit): Option[NonEmptyList[AppliedFragment]] =
      NonEmptyList.fromList(
        List(
          SET.existence.map(sql"c_existence = $existence"),
          SET.name.foldPresent(sql"c_name = ${text_nonempty.opt}"),
          SET.description.foldPresent(sql"c_description = ${text_nonempty.opt}"),
          SET.goa.flatMap(_.proprietaryMonths).map(sql"c_goa_proprietary = $int4_nonneg"),
          SET.goa.flatMap(_.shouldNotify).map(sql"c_goa_should_notify = $bool"),
          SET.goa.flatMap(_.privateHeader).map(sql"c_goa_private_header = $bool"),
          SET.active.flatMap(_.left).map(sql"c_active_start = $date"),
          SET.active.flatMap(_.right).map(sql"c_active_end = $date")
        ).flatten
      )

    def updatePrograms(SET: ProgramPropertiesInput.Edit): Option[AppliedFragment] =
      updates(SET).map { us =>
        void"""
          UPDATE t_program
          SET """ |+| us.intercalate(void", ") |+| void"""
          FROM t_program_update WHERE t_program.c_program_id = t_program_update.c_program_id
          RETURNING t_program.c_program_id
        """
      }


    /** Insert a program, making the passed user PI if it's a non-service user. */
    val InsertProgram: Query[ProgramPropertiesInput.Create, Program.Id] =
      sql"""
        INSERT INTO t_program (
          c_name,
          c_description,
          c_goa_proprietary,
          c_goa_should_notify,
          c_goa_private_header,
          c_existence
        )
        VALUES (
          ${text_nonempty.opt},
          ${text_nonempty.opt},
          $int4_nonneg,
          $bool,
          $bool,
          $existence
        )
        RETURNING c_program_id
      """.query(program_id).contramap { c => (
        c.name,
        c.description,
        c.goa.proprietaryMonths,
        c.goa.shouldNotify,
        c.goa.privateHeader,
        c.existence
      )}

    /** Insert a calibration program, without a user for a staff program */
    val InsertCalibrationProgram: Query[(Option[NonEmptyString], CalibrationRole, String), Program.Id] =
      sql"""
        INSERT INTO t_program (c_name, c_calibration_role, c_library_desc)
        VALUES (${text_nonempty.opt}, $calibration_role, $text)
        RETURNING c_program_id
      """.query(program_id)

  }

}
