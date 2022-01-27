package lucuma.odb.service

import lucuma.core.model.User
import cats.effect.MonadCancelThrow
import cats.syntax.all._
import skunk._
import skunk.codec.all._
import skunk.syntax.all._
import lucuma.core.model.Program
import lucuma.odb.util.Codecs._

trait ProgramService[F[_]] {

  /** Insert a new program, where `userId` is the PI. */
  def insertProgram(name: String, userId: User.Id): F[Program.Id]

}

object ProgramService {

  def fromSession[F[_]: MonadCancelThrow](s: Session[F]): ProgramService[F] =
    new ProgramService[F] {

      def insertProgram(name: String, userId: User.Id): F[Program.Id] =
        for {
          id <- s.prepare(Statements.InsertProgram).use(ps => ps.unique(name))
          _  <- s.prepare(Statements.AddPI).use(pc => pc.execute(id ~ userId))
        } yield id

     }

  object Statements {

    val InsertProgram: Query[String, Program.Id] =
      sql"""
        INSERT INTO t_program (c_name) VALUES ($text)
        RETURNING c_program_id
      """.query(program_id)

    val AddPI: Command[Program.Id ~ User.Id] =
      sql"""
        INSERT INTO t_program_user (c_program_id, c_user_id, c_role)
        VALUES ($program_id, $user_id, 'pi')
      """.command

  }

}