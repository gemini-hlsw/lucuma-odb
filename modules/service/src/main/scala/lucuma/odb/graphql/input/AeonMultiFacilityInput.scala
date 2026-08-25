// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.enums.Instrument
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

object AeonMultiFacilityInput:

  case class Create(requiredInstruments: List[Instrument])

  object Create:
    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          InstrumentBinding.List.Option("requiredInstruments", rInstruments)
        ) => rInstruments.map(is => Create(is.getOrElse(Nil)))

  case class Edit(requiredInstruments: Nullable[List[Instrument]])

  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          InstrumentBinding.List.Nullable("requiredInstruments", rInstruments)
        ) => rInstruments.map(Edit(_))
