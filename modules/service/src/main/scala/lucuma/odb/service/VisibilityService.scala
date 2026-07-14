// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import fs2.Stream
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

/** A single visibility-relevant change: either an observation or a target. */
enum VisibilityChange:
  case ForObservation(id: Observation.Id)
  case ForTarget(id: Target.Id)

/**
 * Exposes the observations and targets whose visibility-relevant inputs have changed.
 *
 * Only observations in workflow state `ready` or `ongoing`, and the targets they use,
 * are considered.
 */
trait VisibilityService[F[_]]:

  /**
   * The observation and target changes invalidated at or after `since` (all
   * observations first, then all targets).
   * The consumer reconstructs (observation, target) pairs itself.
   */
  def selectVisibilityChanges(
    since: Timestamp
  )(using Transaction[F], Services.ServiceAccess): Stream[F, VisibilityChange]

object VisibilityService:

  def instantiate[F[_]](using Services[F]): VisibilityService[F] =
    new VisibilityService[F]:

      override def selectVisibilityChanges(
        since: Timestamp
      )(using Transaction[F], Services.ServiceAccess): Stream[F, VisibilityChange] =
        session.stream(Statements.SelectObservations)(since, 1024).map(VisibilityChange.ForObservation(_)) ++
          session.stream(Statements.SelectTargets)(since, 1024).map(VisibilityChange.ForTarget(_))

  object Statements:

    val SelectObservations: Query[Timestamp, Observation.Id] =
      sql"""
        SELECT ov.c_observation_id
        FROM t_observation_visibility ov
        JOIN t_obscalc oc ON oc.c_observation_id = ov.c_observation_id
        WHERE oc.c_workflow_state IN ('ready', 'ongoing')
          AND ov.c_last_visibility_invalidation >= $core_timestamp
      """.query(observation_id)

    val SelectTargets: Query[Timestamp, Target.Id] =
      sql"""
        SELECT DISTINCT tv.c_target_id
        FROM t_target_visibility tv
        JOIN t_asterism_target a ON a.c_target_id = tv.c_target_id
        JOIN t_obscalc oc ON oc.c_observation_id = a.c_observation_id
        WHERE oc.c_workflow_state IN ('ready', 'ongoing')
          AND tv.c_last_visibility_invalidation >= $core_timestamp
      """.query(target_id)
