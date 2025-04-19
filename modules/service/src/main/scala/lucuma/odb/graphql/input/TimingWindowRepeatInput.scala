// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.*

case class TimingWindowRepeatInput(
  period: TimeSpan,
  times: Option[PosInt]
)

object TimingWindowRepeatInput:
  val Binding: Matcher[TimingWindowRepeatInput] =
    ObjectFieldsBinding.rmap {
      case List(
        TimeSpanInput.Binding("period", rPeriod),
        PosIntBinding.Option("times", rTimes)
      ) => (rPeriod, rTimes).parMapN(TimingWindowRepeatInput(_, _))
    }
