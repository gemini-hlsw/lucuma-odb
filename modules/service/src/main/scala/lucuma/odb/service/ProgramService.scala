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

trait ProgramService[F[_]] {

  /** Insert a new program, where `userId` is the PI. */
  def insertProgram(name: Option[NonEmptyString], userId: User.Id): F[Program.Id]

  /** Update a program, where `userId` is the current user. */
  def updateProgram(
    programId: Program.Id,
    ex: Option[Existence],
    name: Nullable[NonEmptyString],
    userId: User
  ): F[UpdateResult[Program.Id]]

  /** Delete a program with the given id, returning true if deleted, false if not found. */
  def deleteProgram(programId: Program.Id): F[Boolean]

}

object ProgramService {

  def fromSession[F[_]: MonadCancelThrow](s: Session[F]): ProgramService[F] =
    new ProgramService[F] {

      def insertProgram(name: Option[NonEmptyString], userId: User.Id): F[Program.Id] =
        s.transaction.use { _ =>
          for {
            id <- s.prepare(Statements.InsertProgram).use(ps => ps.unique(name))
            _  <- s.prepare(Statements.AddPI).use(pc => pc.execute(id ~ userId))
          } yield id
        }

      def updateProgram(
        programId: Program.Id,
        ex: Option[Existence],
        name: Nullable[NonEmptyString],
        user: User
      ): F[UpdateResult[Program.Id]] =
        Statements.updateProgram(programId, ex, name, user) match {
          case None => UpdateResult.NothingToBeDone.pure[F].widen
          case Some(af) =>
            s.prepare(af.fragment.command).use(_.execute(af.argument)).flatMap {
              case Completion.Update(0) => UpdateResult.NoSuchObject.pure[F].widen
              case Completion.Update(1) => UpdateResult.Success(programId).pure[F].widen
              case other => MonadCancelThrow[F].raiseError(new RuntimeException(s"Expected `Update(0)` or `Update(1)`, found $other."))
            }
        }

      def deleteProgram(programId: Program.Id): F[Boolean] =
        s.prepare(Statements.DeleteProgram).use(pc => pc.execute(programId))
          .flatMap {
            case Completion.Delete(0) => false.pure[F]
            case Completion.Delete(1) => true.pure[F]
            case other => MonadCancelThrow[F].raiseError(new RuntimeException(s"Expected `Delete(0)` or `Delete(1)`, found $other."))
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
              where t_program.c_program_id = $program_id
              and   pu.c_program_id = t_program.c_program_id
              and   pu.c_user_id = $user_id
              and   pu.c_role in ('pi', 'coi', 'support')
            """.apply(programId ~ user.id)

        }

        base |+| up |+| where

      }
    }

    val InsertProgram: Query[Option[NonEmptyString], Program.Id] =
      sql"""
        INSERT INTO t_program (c_name) VALUES (${text_nonempty.opt})
        RETURNING c_program_id
      """.query(program_id)

    val AddPI: Command[Program.Id ~ User.Id] =
      sql"""
        INSERT INTO t_program_user (c_program_id, c_user_id, c_role)
        VALUES ($program_id, $user_id, 'pi')
      """.command

    val DeleteProgram: Command[Program.Id] =
      sql"""
        DELETE FROM t_program
        WHERE c_program_id = $program_id
      """.command
  }

}