// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.ScienceBand
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.*

/** The time, always positive, a TAC recommends for a science band. */
case class TacRecommendedTimeInput(
  band: ScienceBand,
  time: TimeSpan
)

object TacRecommendedTimeInput:

  val Binding: Matcher[TacRecommendedTimeInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ScienceBandBinding("band", rBand),
        TimeSpanInput.Binding("time", rTime)
      ) =>
        val rTimeʹ = rTime.flatMap: t =>
          if t.toNonNegMicroseconds.value > 0 then t.success
          else Result.failure("Recommended times must be greater than zero.")
        (rBand, rTimeʹ).parMapN(apply)
