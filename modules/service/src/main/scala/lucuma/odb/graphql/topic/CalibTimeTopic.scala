// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.topic

import cats.effect.*
import cats.effect.std.Supervisor
import cats.implicits.*
import fs2.Stream
import fs2.concurrent.Topic
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.Gid
import lucuma.odb.data.EditType
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.implicits.*

object CalibTimeTopic {

  /**
   * @param observationId the id of the observation that was inserted or edited
   * @param programId the observation's program's id
   * @param editType determines creation vs update
   */
  case class Element(
    programId:     Program.Id,
    observationId: Observation.Id,
    editType:      EditType,
  )

  /** Infinite stream of observation id, program id and edit type. */
  def updates[F[_]: Logger](s: Session[F], maxQueued: Int): Stream[F, Element] =
    s.channel(id"ch_calib_obs_time").listen(maxQueued).flatMap { n =>
      n.value.split(",") match {
        case Array(pid, oid, u) =>
          (Gid[Program.Id].fromString.getOption(pid), Gid[Observation.Id].fromString.getOption(oid), EditType.fromTgOp(u)) match {
            case (Some(pid), Some(oid), Some(up)) =>
              Stream(Element(pid, oid, up))
            case _                                =>
              Stream.exec(Logger[F].warn(s"Invalid observation and/or event: $n"))
          }
        case _ => Stream.exec(Logger[F].warn(s"Invalid observation and/or event: $n"))
      }
    }

  def elements[F[_]: Concurrent: Logger](
    s:         Session[F],
    maxQueued: Int,
  ): Stream[F, Element] =
    for {
      oid   <- updates(s, maxQueued)
      _     <- Stream.eval(Logger[F].info(s"CalibTimeChannel: $oid"))
    } yield oid

  def apply[F[_]: Concurrent: Logger](
    s:         Session[F],
    maxQueued: Int,
    sup:       Supervisor[F]
  ): F[Topic[F, Element]] =
    for {
      top <- Topic[F, Element]
      els  = elements(s, maxQueued).through(top.publish)
      _   <- sup.supervise(els.compile.drain.onError { case e => Logger[F].error(e)("Calibration Time Event Stream crashed!") } >> Logger[F].info("Calibration Time Event Stream terminated.") )
      _   <- Logger[F].info("Started topic for ch_calib_obs_time")
    } yield top

}
