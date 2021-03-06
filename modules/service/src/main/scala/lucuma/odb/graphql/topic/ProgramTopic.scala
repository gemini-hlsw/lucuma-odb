// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.topic

import cats.effect._
import cats.effect.std.Supervisor
import cats.implicits._
import fs2.Stream
import fs2.concurrent.Topic
import lucuma.core.model.Access.Admin
import lucuma.core.model.Access.Guest
import lucuma.core.model.Access.Ngo
import lucuma.core.model.Access.Pi
import lucuma.core.model.Access.Service
import lucuma.core.model.Access.Staff
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.odb.util.Codecs._
import org.typelevel.log4cats.Logger
import skunk.Query
import skunk._
import skunk.implicits._

object ProgramTopic {

  /**
   * @param programId the id of the program that was inserted or edited
   * @param users users associated with this program
   */
  case class Element(
    programId: Program.Id,
    users:     List[User.Id],
    // TODO: time allocation
  ) {
    def canRead(u: User): Boolean =
      u.role.access match {
        case Admin | Service | Staff => true
        case Ngo                     => ??? // TODO
        case Guest | Pi              => users.contains(u.id)
      }
  }

  /** Infinite stream of program ids. */
  def updates[F[_]: Logger](s: Session[F], maxQueued: Int): Stream[F, Program.Id] =
    s.channel(id"ch_program_edit").listen(maxQueued).flatMap { n =>
      Gid[Program.Id].fromString.getOption(n.value) match {
        case Some(pid) => Stream(pid)
        case None      => Stream.exec(Logger[F].warn(s"Invalid Program.Id in $n"))
      }
    }

  def SelectProgramUsers: Query[Program.Id, User.Id] =
    sql"""
      select c_pi_user_id from t_program where c_program_id = $program_id
      and c_pi_user_id is not null
      union
      select c_user_id from t_program_user where c_program_id = $program_id
      """
      .query(user_id)
      .contramap(pid => pid ~ pid)

  def elements[F[_]: Concurrent: Logger](
    s: Session[F],
    maxQueued: Int,
  ): Stream[F, Element] =
    for {
      pq    <- Stream.resource(s.prepare(SelectProgramUsers))
      pid   <- updates(s, maxQueued)
      users <- Stream.eval(pq.stream(pid, 1024).compile.toList)
      elem   = Element(pid, users)
      _     <- Stream.eval(Logger[F].info(s"ProgramChannel: $elem"))
    } yield elem

  def apply[F[_]: Concurrent: Logger](
    s: Session[F],
    maxQueued: Int,
    sup: Supervisor[F]
  ): F[Topic[F, Element]] =
    for {
      top <- Topic[F, Element]
      els  = elements(s, maxQueued).through(top.publish)
      _   <- sup.supervise(els.compile.drain.onError { case e => Logger[F].error(e)("Stream crashed!") } >> Logger[F].info("Stream terminated.") )
      _   <- Logger[F].info("Started topic for ch_program_edit")
    } yield top

}