// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.*

object TimeSpanInput {

  val Binding: Matcher[TimeSpan] =
    ObjectFieldsBinding.rmap {
      case List(
            TimeSpanBinding.Microseconds.Option("microseconds", rMicroseconds),
            TimeSpanBinding.Milliseconds.Option("milliseconds", rMilliseconds),
            TimeSpanBinding.Seconds.Option("seconds", rSeconds),
            TimeSpanBinding.Minutes.Option("minutes", rMinutes),
            TimeSpanBinding.Hours.Option("hours", rHours),
            TimeSpanBinding.Iso.Option("iso", rIso)
          ) =>
        (rMicroseconds, rMilliseconds, rSeconds, rMinutes, rHours, rIso).parTupled.flatMap {
          case (microseconds, milliseconds, seconds, minutes, hours, iso) =>
            oneOrFail(
              microseconds -> "microseconds",
              milliseconds -> "milliseconds",
              seconds      -> "seconds",
              minutes      -> "minutes",
              hours        -> "hours",
              iso          -> "iso"
            )
        }
    }
}
