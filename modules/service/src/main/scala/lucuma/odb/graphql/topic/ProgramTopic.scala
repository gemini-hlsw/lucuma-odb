// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.topic

import cats.Eq
import cats.derived.*
import cats.effect.*
import cats.effect.std.Supervisor
import cats.implicits.*
import fs2.concurrent.Topic
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.odb.data.EditType
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.implicits.*

object ProgramTopic:

  /**
   * @param programId the id of the program that was inserted or edited
   * @param users users associated with this program
   */
  case class Element(
    programId: Program.Id,
    editType:  EditType,
    users:     List[User.Id]
  ) extends TopicElement derives Eq

  private val topic =
    OdbTopic.define[(Program.Id, EditType), Element](
      "Program",
      id"ch_program_edit",
      _._1,
      (update, users) => Element(update._1, update._2, users)
    ) {
      case Array(_pid, _tg_op) => (Gid[Program.Id].fromString.getOption(_pid), EditType.fromTgOp(_tg_op)).tupled
    }

  def apply[F[_]: Concurrent: Logger](
    s: Session[F],
    maxQueued: Int,
    sup: Supervisor[F]
  ): F[Topic[F, Element]] =
    topic.create(s, maxQueued, sup)