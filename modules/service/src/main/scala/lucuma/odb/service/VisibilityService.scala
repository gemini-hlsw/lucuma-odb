// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Functor
import cats.syntax.foldable.*
import cats.syntax.functor.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.numeric.int4
import skunk.implicits.*

import Services.Syntax.*

/** A single visibility-relevant change: either an observation or a target, with the time it was last invalidated. */
enum VisibilityChange:
  case ForObservation(id: Observation.Id, invalidatedAt: Timestamp)
  case ForTarget(id: Target.Id, invalidatedAt: Timestamp)

/**
 * Exposes the observations and targets whose visibility-relevant inputs have changed,
 * regardless of workflow state. Only soft-deleted observations/targets are excluded.
 *
 * Results are returned in pages ordered by id so that callers can read a large
 * result in a series of short transactions, each page fully materialized.  The
 * pages of one result should share an `until` fixed before the first is read,
 * so that rows stamped while paging are left out of every page and the newest
 * time in the result can't pass a change to a row that was already paged.
 */
trait VisibilityService[F[_]]:

  /**
   * Up to `limit` observations invalidated in [`since`, `until`], ordered by id,
   * starting after observation `after` (or from the beginning if `None`).
   */
  def selectObservationChanges(
    since: Timestamp,
    until: Timestamp,
    after: Option[Observation.Id],
    limit: PosInt
  )(using Transaction[F], Services.ServiceAccess): F[List[VisibilityChange.ForObservation]]

  /**
   * Up to `limit` targets invalidated in [`since`, `until`] that appear in the
   * asterism of at least one present observation, ordered by id, starting after
   * target `after` (or from the beginning if `None`).  The consumer
   * reconstructs (observation, target) pairs itself.
   */
  def selectTargetChanges(
    since: Timestamp,
    until: Timestamp,
    after: Option[Target.Id],
    limit: PosInt
  )(using Transaction[F], Services.ServiceAccess): F[List[VisibilityChange.ForTarget]]

object VisibilityService:

  def instantiate[F[_]: Functor](using Services[F]): VisibilityService[F] =
    new VisibilityService[F]:

      override def selectObservationChanges(
        since: Timestamp,
        until: Timestamp,
        after: Option[Observation.Id],
        limit: PosInt
      )(using Transaction[F], Services.ServiceAccess): F[List[VisibilityChange.ForObservation]] =
        val af = Statements.selectObservations(since, until, after, limit)
        session
          .execute(af.fragment.query(observation_id *: core_timestamp))(af.argument)
          .map(_.map(VisibilityChange.ForObservation.apply))

      override def selectTargetChanges(
        since: Timestamp,
        until: Timestamp,
        after: Option[Target.Id],
        limit: PosInt
      )(using Transaction[F], Services.ServiceAccess): F[List[VisibilityChange.ForTarget]] =
        val af = Statements.selectTargets(since, until, after, limit)
        session
          .execute(af.fragment.query(target_id *: core_timestamp))(af.argument)
          .map(_.map(VisibilityChange.ForTarget.apply))

  object Statements:

    // The cursor condition is added only when there is a cursor, so that first
    // and later pages are separate statements.  A single statement with an
    // optional cursor ("$after IS NULL OR id > $after") gets a generic plan
    // that scans every row before the cursor once Postgres stops planning it
    // for each execution.  The cursor is repeated on the joined table because
    // Postgres doesn't carry an inequality across a join, and would otherwise
    // read that table from its start to line it up.

    def selectObservations(
      since: Timestamp,
      until: Timestamp,
      after: Option[Observation.Id],
      limit: PosInt
    ): AppliedFragment =
      sql"""
        SELECT ov.c_observation_id, ov.c_last_visibility_invalidation
        FROM t_observation_visibility ov
        JOIN t_observation o ON o.c_observation_id = ov.c_observation_id
        WHERE o.c_existence = 'present'
          AND ov.c_last_visibility_invalidation >= $core_timestamp
          AND ov.c_last_visibility_invalidation <= $core_timestamp
      """.apply((since, until)) |+|
      after.foldMap: a =>
        sql"""
          AND ov.c_observation_id > $observation_id
          AND o.c_observation_id > $observation_id
        """.apply((a, a))
      |+|
      sql"""
        ORDER BY ov.c_observation_id
        LIMIT $int4
      """.apply(limit.value)

    def selectTargets(
      since: Timestamp,
      until: Timestamp,
      after: Option[Target.Id],
      limit: PosInt
    ): AppliedFragment =
      sql"""
        SELECT tv.c_target_id, tv.c_last_visibility_invalidation
        FROM t_target_visibility tv
        JOIN t_target t ON t.c_target_id = tv.c_target_id
        WHERE t.c_existence = 'present'
          AND tv.c_last_visibility_invalidation >= $core_timestamp
          AND tv.c_last_visibility_invalidation <= $core_timestamp
          AND EXISTS (
            SELECT 1
            FROM t_asterism_target a
            JOIN t_observation o ON o.c_observation_id = a.c_observation_id
            WHERE a.c_target_id = tv.c_target_id
              AND o.c_existence = 'present'
          )
      """.apply((since, until)) |+|
      after.foldMap: a =>
        sql"""
          AND tv.c_target_id > $target_id
          AND t.c_target_id > $target_id
        """.apply((a, a))
      |+|
      sql"""
        ORDER BY tv.c_target_id
        LIMIT $int4
      """.apply(limit.value)
