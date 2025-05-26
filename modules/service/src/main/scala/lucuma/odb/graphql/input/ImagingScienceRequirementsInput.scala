// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.ScienceMode
import lucuma.core.math.Angle
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding

case class ImagingScienceRequirementsInput(
  minimumFov:      Nullable[Angle],
  narrowFilters:   Option[Boolean],
  broadFilters:    Option[Boolean],
  combinedFilters: Option[Boolean]
) {
  val scienceMode: ScienceMode = ScienceMode.Imaging
}

object ImagingScienceRequirementsInput:

  val Default: ImagingScienceRequirementsInput =
    ImagingScienceRequirementsInput(
      minimumFov       = Nullable.Null,
      narrowFilters    = None,
      broadFilters     = None,
      combinedFilters  = None
    )

  val Binding: Matcher[ImagingScienceRequirementsInput] =
    ObjectFieldsBinding.rmap {
      case List(
        AngleInput.Binding.Nullable("minimumFov", rMinimumFov),
        BooleanBinding.Option("narrowFilters", rNarrowFilter),
        BooleanBinding.Option("broadFilters", rBroadFilter),
        BooleanBinding.Option("combinedFilters", rCombinedFilter)
      ) =>
        (rMinimumFov, rNarrowFilter, rBroadFilter, rCombinedFilter)
          .parTupled.map(
            (minimumFov, narrowFilter, broadFilter, combinedFilter) =>
              ImagingScienceRequirementsInput(
                minimumFov,
                narrowFilter,
                broadFilter,
                combinedFilter
              )
            )
    }
