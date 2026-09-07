// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.topic

import cats.effect.*
import cats.effect.std.Supervisor
import cats.implicits.*
import fs2.Stream
import fs2.concurrent.Topic
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.odb.data.EditType
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer
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
    users:     List[User.Id],
  ) extends TopicElement

  private val topic =
    OdbTopic.define[(Program.Id, EditType), Element](
      "Program",
      ident"ch_program_edit",
      _._1,
      (update, users) => Element(update._1, update._2, users)
    ) {
      case Array(_pid, _tg_op) => (Gid[Program.Id].fromString.getOption(_pid), EditType.fromTgOp(_tg_op)).tupled
    }

  def create[F[_]: Concurrent: Logger](sup: Supervisor[F]): F[Topic[F, Element]] =
    topic.create(sup)

  def feed[F[_]: Concurrent: Logger: Tracer](
    s:         Session[F],
    maxQueued: Int,
    top:       Topic[F, Element]
  ): Stream[F, Unit] =
    topic.feed(s, maxQueued, top)
