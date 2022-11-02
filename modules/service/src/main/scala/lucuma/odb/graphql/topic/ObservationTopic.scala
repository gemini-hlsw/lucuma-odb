// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.odb.data.EditType
import lucuma.odb.util.Codecs.*
import org.typelevel.log4cats.Logger
import skunk.Query
import skunk.*
import skunk.implicits.*

object ObservationTopic {

  /**
   * @param observationId the id of the observation that was inserted or edited
   * @param programId the observation's program's id
   * @param eventId serial event id
   * @param editType determines creation vs update
   * @param users users associated with this program
   */
  case class Element(
    observationId: Observation.Id,
    programId:     Program.Id,
    eventId:       Long,
    editType:      EditType,
    users:         List[User.Id]
  ) {

    // Same as ProgramTopic `canRead` ...

    def canRead(u: User): Boolean =
      u.role.access match {
        case Admin | Service | Staff => true
        case Ngo                     => ??? // TODO
        case Guest | Pi              => users.contains(u.id)
      }

  }

  /** Infinite stream of observation id, program id, event id, and edit type. */
  def updates[F[_]: Logger](s: Session[F], maxQueued: Int): Stream[F, (Observation.Id, Program.Id, Long, EditType)] =
    s.channel(id"ch_observation_edit").listen(maxQueued).flatMap { n =>
      n.value.split(",") match {
        case Array(_oid, _pid, _eid, _tg_op) =>
          (Gid[Observation.Id].fromString.getOption(_oid), Gid[Program.Id].fromString.getOption(_pid), _eid.toLongOption, EditType.fromTgOp(_tg_op)).tupled match {
            case Some(tuple) => Stream(tuple)
            case None        => Stream.exec(Logger[F].warn(s"Invalid observation and/or event: $n"))
          }
        case _ => Stream.exec(Logger[F].warn(s"Invalid observation and/or event: $n"))
      }
    }

  // Same query in ProgramTopic ...

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
    s:         Session[F],
    maxQueued: Int,
  ): Stream[F, Element] =
    for {
      pq    <- Stream.resource(s.prepare(SelectProgramUsers))
      oid   <- updates(s, maxQueued)
      users <- Stream.eval(pq.stream(oid._2, 1024).compile.toList)
      elem   = Element(oid._1, oid._2, oid._3, oid._4, users)
      _     <- Stream.eval(Logger[F].info(s"ObservationChannel: $elem"))
    } yield elem

  def apply[F[_]: Concurrent: Logger](
    s:         Session[F],
    maxQueued: Int,
    sup:       Supervisor[F]
  ): F[Topic[F, Element]] =
    for {
      top <- Topic[F, Element]
      els  = elements(s, maxQueued).through(top.publish)
      _   <- sup.supervise(els.compile.drain.onError { case e => Logger[F].error(e)("Observation Event Stream crashed!") } >> Logger[F].info("Observation Event Stream terminated.") )
      _   <- Logger[F].info("Started topic for ch_observation_edit")
    } yield top

}