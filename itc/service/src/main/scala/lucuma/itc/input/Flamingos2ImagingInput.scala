// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import lucuma.core.enums.Flamingos2Filter
import lucuma.odb.graphql.binding.*

case class Flamingos2ImagingInput(filter: Flamingos2Filter) extends InstrumentModesInput

object Flamingos2ImagingInput:
  val binding: Matcher[Flamingos2ImagingInput] =
    ObjectFieldsBinding.rmap:
      case List(Flamingos2FilterBinding("filter", filter)) =>
        filter.map(f => Flamingos2ImagingInput(f))
