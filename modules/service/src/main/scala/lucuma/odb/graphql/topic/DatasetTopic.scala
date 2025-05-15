// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.topic

import cats.effect.Concurrent
import cats.effect.std.Supervisor
import cats.syntax.all.*
import fs2.concurrent.Topic
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.Gid
import lucuma.odb.data.EditType
import natchez.Trace
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.syntax.stringcontext.*

object DatasetTopic:

  case class Element(
    datasetId:     Dataset.Id,
    observationId: Observation.Id,
    programId:     Program.Id,
    isWritten:     Boolean,
    editType:      EditType,
    users:         List[User.Id]
  ) extends TopicElement

  private val topic =
    OdbTopic.define[(Dataset.Id, Observation.Id, Program.Id, Boolean, EditType), Element](
      "Dataset",
      id"ch_dataset_edit",
      _._3,
      (update, users) => Element(update._1, update._2, update._3, update._4, update._5, users)
    ) {
      case Array(_did, _oid, _pid, _isWritten, _tg_op) =>
        (
          Gid[Dataset.Id].fromString.getOption(_did),
          Gid[Observation.Id].fromString.getOption(_oid),
          Gid[Program.Id].fromString.getOption(_pid),
          "true".equalsIgnoreCase(_isWritten).some,
          EditType.fromTgOp(_tg_op)
        ).tupled
    }

  def apply[F[_]: Concurrent: Logger: Trace](
    s:         Session[F],
    maxQueued: Int,
    sup:       Supervisor[F]
  ): F[Topic[F, Element]] =
    topic.create(s, maxQueued, sup)