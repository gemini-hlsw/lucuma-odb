// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.topic

import cats.effect.*
import cats.effect.std.Supervisor
import cats.implicits.*
import fs2.concurrent.Topic
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.odb.data.EditType
import natchez.Trace
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.implicits.*

object TargetTopic:

  /**
   * @param targetId the id of the target that was inserted or edited
   * @param programId the target's program's id
   * @param eventId serial event id
   * @param editType determines creation vs update
   * @param users users associated with this program
   */
  case class Element(
    targetId:  Target.Id,
    programId: Program.Id,
    editType:  EditType,
    users:     List[User.Id]
  ) extends TopicElement

  private val topic =
    OdbTopic.define[(Target.Id, Program.Id, EditType), Element](
      "Target",
      id"ch_target_edit",
      _._2,
      (update, users) => Element(update._1, update._2, update._3, users)
    ) {
      case Array(_oid, _pid, _tg_op) =>
        (
          Gid[Target.Id].fromString.getOption(_oid),
          Gid[Program.Id].fromString.getOption(_pid),
          EditType.fromTgOp(_tg_op)
        ).tupled
    }

  def apply[F[_]: Concurrent: Logger: Trace](
    s:         Session[F],
    maxQueued: Int,
    sup:       Supervisor[F]
  ): F[Topic[F, Element]] =
    topic.create(s, maxQueued, sup)