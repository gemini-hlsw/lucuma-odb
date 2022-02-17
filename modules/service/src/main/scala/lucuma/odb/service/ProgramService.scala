package lucuma.odb.service

import lucuma.core.model.User
import cats.effect.MonadCancelThrow
import cats.syntax.all._
import skunk._
import skunk.syntax.all._
import lucuma.core.model.Program
import lucuma.odb.util.Codecs._
import skunk.data.Completion
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.odb.data._
import cats.data.NonEmptyList
import lucuma.core.model.Access.Ngo
import lucuma.core.model.Access.Service
import lucuma.core.model.Access.Guest
import lucuma.core.model.Access.Admin
import lucuma.core.model.Access.Pi
import lucuma.core.model.Access.Staff
import lucuma.odb.data.Nullable.Absent
import lucuma.odb.data.Nullable.NonNull
import lucuma.core.model.ServiceUser
import natchez.Trace

trait ProgramService[F[_]] {

  /**
   * Insert a new program, where the calling user becomes PI (unless it's a Service user, in which
   * case the PI is left empty.
   */
  def insertProgram(name: Option[NonEmptyString]): F[Program.Id]

  /** Update a program, where `userId` is the current user. */
  def updateProgram(
    programId: Program.Id,
    ex: Option[Existence],
    name: Nullable[NonEmptyString],
  ): F[UpdateResult[Program.Id]]

  /** Delete a program with the given id, returning true if deleted, false if not found. */
  def deleteProgram(programId: Program.Id): F[Boolean]

}

object ProgramService {

  /**
   * Construct a `ProgramService` using the specified `Session`, for the specified `User`. All
   * operations will be performed on behalf of `user`.
   */
  def fromSessionAndUser[F[_]: MonadCancelThrow: Trace](s: Session[F], user: User): ProgramService[F] =
    new ProgramService[F] {

      def fail[A](msg: String): F[A] =
        MonadCancelThrow[F].raiseError(new RuntimeException(msg))

      def insertProgram(name: Option[NonEmptyString]): F[Program.Id] =
        Trace[F].span("insertProgram") {
          s.prepare(Statements.InsertProgram).use(ps => ps.unique(name ~ user))
        }

      def updateProgram(
        programId: Program.Id,
        ex:        Option[Existence],
        name:      Nullable[NonEmptyString],
      ): F[UpdateResult[Program.Id]] =
        Trace[F].span("updateProgram") {
          Statements.updateProgram(programId, ex, name, user) match {
            case None => UpdateResult.NothingToBeDone.pure[F].widen
            case Some(af) =>
              s.prepare(af.fragment.command).use(_.execute(af.argument)).flatMap {
                case Completion.Update(0) => UpdateResult.NoSuchObject.pure[F].widen
                case Completion.Update(1) => UpdateResult.Success(programId).pure[F].widen
                case other => fail(s"Expected `Update(0)` or `Update(1)`, found $other.")
              }
          }
        }

      def deleteProgram(programId: Program.Id): F[Boolean] =
        Trace[F].span("deleteProgram") {
          s.prepare(Statements.DeleteProgram).use(pc => pc.execute(programId))
            .flatMap {
              case Completion.Delete(0) => false.pure[F]
              case Completion.Delete(1) => true.pure[F]
              case other => fail(s"Expected `Delete(0)` or `Delete(1)`, found $other.")
            }
          }

     }

  object Statements {

    def updateProgram(
      programId: Program.Id,
      ex: Option[Existence],
      name: Nullable[NonEmptyString],
      user: User
    ): Option[AppliedFragment] = {

      val base = void"update t_program set "

      val upExistence = sql"c_existence = $existence"
      val upName      = sql"c_name = ${text_nonempty.opt}"

      val ups: List[AppliedFragment] =
        List(
          ex.map(upExistence),
          name match {
            case Nullable.Null => Some(upName(None))
            case Absent => None
            case NonNull(value) => Some(upName(Some(value)))
          }
        ).flatten

      NonEmptyList.fromList(ups).map { nel =>

        val up = nel.intercalate(void", ")

        val where = user.role.access match {

          case Service | Admin | Staff =>
            sql"""
              where p.c_program_id = $program_id
            """.apply(programId)

          case Ngo     => ??? // TODO

          case Guest | Pi =>
            sql"""
              from  t_program_user pu
              where (t_program.c_pi_user_id = $user_id) or (
                 t_program.c_program_id = $program_id
                 and   pu.c_program_id = t_program.c_program_id
                 and   pu.c_user_id = $user_id
                 and   pu.c_role in ('coi', 'support')
              )
            """.apply(user.id ~ programId ~ user.id)

        }

        base |+| up |+| where

      }
    }

    /** Insert a program, making the passed user PI if it's a non-service user. */
    val InsertProgram: Query[Option[NonEmptyString] ~ User, Program.Id] =
      sql"""
        INSERT INTO t_program (c_name, c_pi_user_id, c_pi_user_type)
        VALUES (${text_nonempty.opt}, ${(user_id ~ user_type).opt})
        RETURNING c_program_id
      """.query(program_id)
         .contramap {
            case oNes ~ ServiceUser(_, _) => oNes ~ None
            case oNes ~ nonServiceUser    => oNes ~ Some(nonServiceUser.id ~ UserType.fromUser(nonServiceUser))
         }


    val DeleteProgram: Command[Program.Id] =
      sql"""
        DELETE FROM t_program
        WHERE c_program_id = $program_id
      """.command
  }

}