// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.*

object TimeSpanInput {

  val Binding: Matcher[TimeSpan] =
    OneOfBinding(
      "microseconds" -> TimeSpanBinding.Microseconds,
      "milliseconds" -> TimeSpanBinding.Milliseconds,
      "seconds"      -> TimeSpanBinding.Seconds,
      "minutes"      -> TimeSpanBinding.Minutes,
      "hours"        -> TimeSpanBinding.Hours,
      "iso"          -> TimeSpanBinding.Iso
    )
}
