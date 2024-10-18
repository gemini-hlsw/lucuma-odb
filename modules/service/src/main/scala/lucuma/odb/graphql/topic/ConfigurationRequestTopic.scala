// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.topic

import cats.effect.*
import cats.effect.std.Supervisor
import cats.implicits.*
import fs2.Stream
import fs2.concurrent.Topic
import lucuma.core.model.Access.Admin
import lucuma.core.model.Access.Guest
import lucuma.core.model.Access.Ngo
import lucuma.core.model.Access.Pi
import lucuma.core.model.Access.Service
import lucuma.core.model.Access.Staff
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.odb.data.EditType
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.implicits.*

object ConfigurationRequestTopic {

  case class Element(
    configurationRequestId: ConfigurationRequest.Id,
    programId: Program.Id,
    editType: EditType,
    users: List[User.Id]
  ) {

    def canRead(u: User): Boolean =
      u.role.access match {
        case Admin | Service | Staff => true
        case Ngo                     => ??? // TODO
        case Guest | Pi              => users.contains(u.id)
      }

  }

  /** Infinite stream of group id, program id, event id, and edit type. */
  def updates[F[_]: Logger](s: Session[F], maxQueued: Int): Stream[F, (ConfigurationRequest.Id, Program.Id, EditType)] =
    s.channel(id"ch_configuration_request_edit").listen(maxQueued).flatMap { n =>
      n.value.split(",") match {
        case Array(_oid, _pid, _tg_op) =>
          (Gid[ConfigurationRequest.Id].fromString.getOption(_oid), Gid[Program.Id].fromString.getOption(_pid), EditType.fromTgOp(_tg_op)) match {
            case (Some(gid), Some(pid), Some(op)) => Stream((gid, pid, op))
            case _                          => Stream.exec(Logger[F].warn(s"Invalid group and/or event: $n"))
          }
        case _ => Stream.exec(Logger[F].warn(s"Invalid group and/or event: $n"))
      }
    }

  def elements[F[_]: Concurrent: Logger](
    s:         Session[F],
    maxQueued: Int,
  ): Stream[F, Element] =
    for {
      oid   <- updates(s, maxQueued)
      users <- Stream.eval(ProgramTopic.selectProgramUsers(s, oid._2))
      elem   = Element(oid._1, oid._2, oid._3, users)
      _     <- Stream.eval(Logger[F].warn(s"Configuration Request Channel: $elem"))
    } yield elem

  def apply[F[_]: Concurrent: Logger](
    s:         Session[F],
    maxQueued: Int,
    sup:       Supervisor[F]
  ): F[Topic[F, Element]] =
    for {
      top <- Topic[F, Element]
      els  = elements(s, maxQueued).through(top.publish)
      _   <- sup.supervise(els.compile.drain.onError { case e => Logger[F].error(e)("Configuration Request  Event Stream crashed!") } >> Logger[F].info("Group Event Stream terminated.") )
      _   <- Logger[F].info("Started topic for ch_configuration_request_edit")
    } yield top

}
