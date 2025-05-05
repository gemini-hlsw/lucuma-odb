// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.topic

import cats.derived.*
import cats.effect.Concurrent
import cats.effect.std.Supervisor
import cats.syntax.apply.*
import fs2.concurrent.Topic
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.odb.data.ExecutionEventType
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.implicits.*

object ExecutionEventAddedTopic:
  case class Element(
    exectionEventId: ExecutionEvent.Id,
    programId:       Program.Id,
    observationId:   Observation.Id,
    visitId:         Visit.Id,
    eventType:       ExecutionEventType,
    users:           List[User.Id]
  ) extends TopicElement derives cats.Eq

  private val topic =
    OdbTopic.define[(ExecutionEvent.Id, Program.Id, Observation.Id, Visit.Id, ExecutionEventType), Element](
      "ExecutionEvent",
      id"ch_execution_event_added",
      _._2,
      (update, users) => Element(update._1, update._2, update._3, update._4, update._5, users)
    ) {
      case Array(_eid, _pid, _oid, _vid, _type) =>
        (
          Gid[ExecutionEvent.Id].fromString.getOption(_eid),
          Gid[Program.Id].fromString.getOption(_pid),
          Gid[Observation.Id].fromString.getOption(_oid),
          Gid[Visit.Id].fromString.getOption(_vid),
          Enumerated.fromTag[ExecutionEventType].getOption(_type)
        ).tupled
    }

  def apply[F[_]: Concurrent: Logger](
    s:         Session[F],
    maxQueued: Int,
    sup:       Supervisor[F]
  ): F[Topic[F, Element]] =
    topic.create(s, maxQueued, sup)


