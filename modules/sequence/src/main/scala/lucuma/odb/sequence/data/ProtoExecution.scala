// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.data.NonEmptyList
import monocle.Focus
import monocle.Lens

final case class ProtoExecution[S, D](
  static:      S,
  acquisition: ProtoSequence[D],
  science:     ProtoSequence[D]
)

object ProtoExecution {

  def static[S, D]: Lens[ProtoExecution[S, D], S] =
    Focus[ProtoExecution[S, D]](_.static)

  def acquisition[S, D]: Lens[ProtoExecution[S, D], ProtoSequence[D]] =
    Focus[ProtoExecution[S, D]](_.acquisition)

  def science[S, D]: Lens[ProtoExecution[S, D], ProtoSequence[D]] =
    Focus[ProtoExecution[S, D]](_.science)

}
