// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel._
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

/*
gmosNorthLongSlit: GmosNorthLongSlitInput
gmosSouthLongSlit: GmosSouthLongSlitInput
*/

final case class ObservingModeInput(
  gmosNorthLongSlit: Nullable[GmosNorthLongSlitInput]
)

object ObservingModeInput {

  val Binding: Matcher[ObservingModeInput] =
    ObjectFieldsBinding.rmap {
      case List(
        // TODO: when we add GMOS South, then we need to make the input at most one defined
        GmosNorthLongSlitInput.Binding.Nullable("gmosNorthLongSlit", rGmosNorthLongSlit)
      ) =>
        rGmosNorthLongSlit.map(ObservingModeInput.apply)
    }

}