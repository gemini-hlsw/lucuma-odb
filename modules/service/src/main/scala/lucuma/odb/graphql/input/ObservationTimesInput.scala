// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

case class ObservationTimesInput(
  observationTime:     Nullable[Timestamp],
  observationDuration: Nullable[TimeSpan]
)

object ObservationTimesInput {
  val Empty: ObservationTimesInput =
    ObservationTimesInput(
      observationTime =     Nullable.Absent,
      observationDuration = Nullable.Absent
    )

  val Binding: Matcher[ObservationTimesInput] =
    ObjectFieldsBinding.rmap {
      case List(
        TimestampBinding.Nullable("observationTime", rObservationTime),
        TimeSpanInput.Binding.Nullable("observationDuration", rObservationDuration)
      ) =>
        (rObservationTime, rObservationDuration).parMapN(ObservationTimesInput.apply)
    }
}
