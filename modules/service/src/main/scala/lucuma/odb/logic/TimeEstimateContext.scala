// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.MonadError
import cats.syntax.functor.*
import lucuma.odb.graphql.enums.Enums
import skunk.Session

/**
 * Time estimates loaded from the database upon startup and subsequently used
 * for time estimate calculation.
 */
case class TimeEstimateContext(
  enums:       Enums,
  gmosReadout: GmosReadoutTime
)

object TimeEstimateContext {

  def select[F[_]](s: Session[F], enums: Enums)(using MonadError[F, Throwable]): F[TimeEstimateContext] =
    GmosReadoutTime.load(s).map { g =>
      TimeEstimateContext(enums, g)
    }


}

