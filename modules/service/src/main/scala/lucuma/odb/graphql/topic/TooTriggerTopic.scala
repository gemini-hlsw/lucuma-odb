// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.topic

import cats.effect.*
import cats.effect.std.Supervisor
import cats.implicits.*
import fs2.concurrent.Topic
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.odb.data.EditType
import lucuma.odb.data.TooTrigger
import lucuma.odb.data.TooTriggerStatus
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer
import skunk.*
import skunk.implicits.*

object TooTriggerTopic:

  /**
   * Elements of the ToO trigger edit stream, broadcast on ch_too_trigger_edit.
   *
   * @param triggerId     the trigger that was created or edited
   * @param observationId the observation the trigger is for
   * @param programId     the observation's program's id
   * @param status        the trigger's status after the edit
   * @param editType      creation vs update
   * @param users         users associated with the program (audience for scoping)
   */
  case class Element(
    triggerId:     TooTrigger.Id,
    observationId: Observation.Id,
    programId:     Program.Id,
    status:        TooTriggerStatus,
    editType:      EditType,
    users:         List[User.Id]
  ) extends TopicElement

  private val topic =
    OdbTopic.define[(TooTrigger.Id, Observation.Id, Program.Id, TooTriggerStatus, EditType), Element](
      "TooTrigger",
      id"ch_too_trigger_edit",
      _._3, // program id -> audience is anyone who can read the program
      (u, users) => Element(u._1, u._2, u._3, u._4, u._5, users)
    ) {
      case Array(_tid, _oid, _pid, _status, _tg_op) =>
        (
          TooTrigger.Id.parse(_tid),
          Gid[Observation.Id].fromString.getOption(_oid),
          Gid[Program.Id].fromString.getOption(_pid),
          Enumerated[TooTriggerStatus].fromTag(_status),
          EditType.fromTgOp(_tg_op)
        ).tupled
    }

  def apply[F[_]: Concurrent: Logger: Tracer](
    s:         Session[F],
    maxQueued: Int,
    sup:       Supervisor[F]
  ): F[Topic[F, Element]] =
    topic.create(s, maxQueued, sup)
