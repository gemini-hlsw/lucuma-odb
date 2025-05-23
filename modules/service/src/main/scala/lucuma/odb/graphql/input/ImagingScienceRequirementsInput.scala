// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.ScienceMode
import lucuma.core.math.Angle
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.binding.GmosNorthFilterBinding
import lucuma.odb.graphql.binding.GmosSouthFilterBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding

case class ImagingScienceRequirementsInput(
  minimumFov:      Nullable[Angle],
  narrowFilters:   Nullable[Boolean],
  broadFilters:    Nullable[Boolean],
  combinedFilters: Nullable[Boolean],
  gmosNorth:       Nullable[ImagingGmosNorthScienceRequirementsInput],
  gmosSouth:       Nullable[ImagingGmosSouthScienceRequirementsInput]
) {
  val scienceMode: ScienceMode = ScienceMode.Imaging
}

object ImagingScienceRequirementsInput:

  val Default: ImagingScienceRequirementsInput =
    ImagingScienceRequirementsInput(
      minimumFov       = Nullable.Null,
      narrowFilters    = Nullable.Null,
      broadFilters     = Nullable.Null,
      combinedFilters  = Nullable.Null,
      gmosNorth        = Nullable.Null,
      gmosSouth        = Nullable.Null
    )

  val Binding: Matcher[ImagingScienceRequirementsInput] =
    ObjectFieldsBinding.rmap {
      case List(
        AngleInput.Binding.Nullable("minimumFov", rMinimumFov),
        BooleanBinding.Nullable("narrowFilters", rNarrowFilter),
        BooleanBinding.Nullable("broadFilters", rBroadFilter),
        BooleanBinding.Nullable("combinedFilters", rCombinedFilter),
        ImagingGmosNorthScienceRequirementsInput.Binding.Nullable("gmosNorth", rGmosNorth),
        ImagingGmosSouthScienceRequirementsInput.Binding.Nullable("gmosSouth", rGmosSouth)
      ) =>
        (rMinimumFov, rNarrowFilter, rBroadFilter, rCombinedFilter, rGmosNorth, rGmosSouth)
          .parTupled.flatMap(
            (minimumFov, narrowFilter, broadFilter, combinedFilter, gmosNorth, gmosSouth) =>
            atMostOne(
              gmosNorth.toOption -> "gmosNorth",
              gmosSouth.toOption -> "gmosSouth"
            ).map(_ =>
              ImagingScienceRequirementsInput(minimumFov, narrowFilter, broadFilter, combinedFilter, gmosNorth, gmosSouth)
            ))
    }

case class ImagingGmosNorthScienceRequirementsInput(filters: Option[List[GmosNorthFilter]])

object ImagingGmosNorthScienceRequirementsInput:

  val Default: ImagingGmosNorthScienceRequirementsInput =
    ImagingGmosNorthScienceRequirementsInput(None)

  val Binding: Matcher[ImagingGmosNorthScienceRequirementsInput] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosNorthFilterBinding.List.Option("filters", rFilters),
      ) =>
        rFilters.map(ImagingGmosNorthScienceRequirementsInput.apply)
    }

case class ImagingGmosSouthScienceRequirementsInput(filters: Option[List[GmosSouthFilter]])

object ImagingGmosSouthScienceRequirementsInput:

  val Default: ImagingGmosSouthScienceRequirementsInput =
    ImagingGmosSouthScienceRequirementsInput(None)

  val Binding: Matcher[ImagingGmosSouthScienceRequirementsInput] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosSouthFilterBinding.List.Option("filters", rFilters),
      ) =>
        rFilters.map(ImagingGmosSouthScienceRequirementsInput.apply)
    }
