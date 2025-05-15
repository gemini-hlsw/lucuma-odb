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
import lucuma.core.util.CalculationState
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.odb.data.EditType
import natchez.Trace
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.implicits.*

object ObscalcTopic:

  /**
   * @param observationId the id of the observation that was inserted or edited
   * @param programId the observation's program's id
   * @param oldState the previous obs calculation state (if any)
   * @param newState the current obs calculation state
   * @param editType determines creation vs update
   * @param users users associated with this program
   */
  case class Element(
    observationId: Observation.Id,
    programId:     Program.Id,
    oldState:      Option[CalculationState],
    newState:      Option[CalculationState],
    editType:      EditType,
    users:         List[User.Id]
  ) extends TopicElement

  private val topic =
    OdbTopic.define[(Observation.Id, Program.Id, Option[CalculationState], Option[CalculationState], EditType), Element](
      "Obscalc",
      id"ch_obscalc_update",
      _._2,
      (update, users) => Element(update._1, update._2, update._3, update._4, update._5, users)
    ) {
      case Array(_oid, _pid, _oldState, _newState, _tg_op) =>
        (
          Gid[Observation.Id].fromString.getOption(_oid),
          Gid[Program.Id].fromString.getOption(_pid),
          Enumerated[CalculationState].fromTag(_oldState).some, // treat the string "null" as Some(None)
          Enumerated[CalculationState].fromTag(_newState).some,
          EditType.fromTgOp(_tg_op)
        ).tupled
    }

  def apply[F[_]: Concurrent: Logger: Trace](
    s:         Session[F],
    maxQueued: Int,
    sup:       Supervisor[F]
  ): F[Topic[F, Element]] =
    topic.create(s, maxQueued, sup)