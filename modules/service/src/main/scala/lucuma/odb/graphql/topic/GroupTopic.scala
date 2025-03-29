// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.topic

import cats.Eq
import cats.derived.*
import cats.effect.*
import cats.effect.std.Supervisor
import cats.implicits.*
import fs2.concurrent.Topic
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.odb.data.EditType
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.implicits.*

object GroupTopic:

  /**
   * @param groupId the id of the group that was inserted or edited
   * @param programId the group's program's id
   * @param eventId serial event id
   * @param editType determines creation vs update
   * @param users users associated with this program
   */
  case class Element(
    groupId:   Option[Group.Id],
    programId: Program.Id,
    editType:  EditType,
    users:     List[User.Id]
  ) extends TopicElement derives Eq

  private val topic =
    OdbTopic.define[(Option[Group.Id], Program.Id, EditType), Element](
      "Group",
      id"ch_group_edit",
      _._2,
      (update, users) => Element(update._1, update._2, update._3, users)
    ) {
      case Array(_oid, _pid, _tg_op) =>
        (Gid[Group.Id].fromString.getOption(_oid), Gid[Program.Id].fromString.getOption(_pid), EditType.fromTgOp(_tg_op)) match
          case (gid, Some(pid), Some(op)) => (gid, pid, op).some
          case _                          => none
    }

  def apply[F[_]: Concurrent: Logger](
    s:         Session[F],
    maxQueued: Int,
    sup:       Supervisor[F]
  ): F[Topic[F, Element]] =
    topic.create(s, maxQueued, sup)