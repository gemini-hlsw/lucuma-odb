// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.MonadError
import cats.syntax.functor.*
import lucuma.odb.graphql.enums.Enums
import skunk.Session

/**
 * Planned time duration estimates loaded from the database upon startup and
 * subsequently used for planned time calculation.
 */
case class PlannedTimeContext(
  enums:       Enums,
  gmosReadout: GmosReadoutTime
)

object PlannedTimeContext {

  def select[F[_]](s: Session[F], enums: Enums)(using MonadError[F, Throwable]): F[PlannedTimeContext] =
    GmosReadoutTime.load(s).map { g =>
      PlannedTimeContext(enums, g)
    }


}

