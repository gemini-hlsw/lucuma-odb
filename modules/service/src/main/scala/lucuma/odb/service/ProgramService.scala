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

trait ProgramService[F[_]] {

  /** Insert a new program, where `userId` is the PI. */
  def insertProgram(name: Option[NonEmptyString], userId: User.Id): F[Program.Id]

  /** Delete a program with the given id, returning true if deleted, false if not found. */
  def deleteProgram(programId: Program.Id): F[Boolean]

}

object ProgramService {

  def fromSession[F[_]: MonadCancelThrow](s: Session[F]): ProgramService[F] =
    new ProgramService[F] {

      def insertProgram(name: Option[NonEmptyString], userId: User.Id): F[Program.Id] =
        for {
          id <- s.prepare(Statements.InsertProgram).use(ps => ps.unique(name))
          _  <- s.prepare(Statements.AddPI).use(pc => pc.execute(id ~ userId))
        } yield id

      def deleteProgram(programId: Program.Id): F[Boolean] =
        s.prepare(Statements.DeleteProgram).use(pc => pc.execute(programId))
          .flatMap {
            case Completion.Delete(0) => false.pure[F]
            case Completion.Delete(1) => true.pure[F]
            case other => MonadCancelThrow[F].raiseError(new RuntimeException(s"Expected `Delete(0)` or `Delete(1)`, found $other."))
          }

     }

  object Statements {

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