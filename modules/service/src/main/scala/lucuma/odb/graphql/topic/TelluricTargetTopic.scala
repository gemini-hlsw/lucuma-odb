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
import natchez.Trace
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.implicits.*

object TelluricTargetTopic:

  case class Element(
    observationId: Observation.Id,
    programId:     Program.Id,
    oldState:      Option[CalculationState],
    newState:      Option[CalculationState],
    users:         List[User.Id]
  ) extends TopicElement

  private val topic =
    OdbTopic.define[
      (Observation.Id, Program.Id, Option[CalculationState], Option[CalculationState]),
      Element
    ](
      "TelluricTarget",
      id"ch_telluric_resolution",
      _._2,
      (update, users) => Element(update._1, update._2, update._3, update._4, users)
    ) {
      case Array(_oid, _pid, _oldState, _newState) =>
        (
          Gid[Observation.Id].fromString.getOption(_oid),
          Gid[Program.Id].fromString.getOption(_pid),
          if _oldState == "null" then none.some else Enumerated[CalculationState].fromTag(_oldState).map(_.some),
          Enumerated[CalculationState].fromTag(_newState).map(_.some)
        ).tupled
    }

  def apply[F[_]: Concurrent: Logger: Trace](
    s:         Session[F],
    maxQueued: Int,
    sup:       Supervisor[F]
  ): F[Topic[F, Element]] =
    topic.create(s, maxQueued, sup)
