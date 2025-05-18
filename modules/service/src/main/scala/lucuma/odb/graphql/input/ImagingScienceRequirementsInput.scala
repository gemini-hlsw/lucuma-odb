// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.ScienceMode
import lucuma.core.math.Angle
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.binding.GmosNorthFilterBinding
import lucuma.odb.graphql.binding.GmosSouthFilterBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding

case class ImagingScienceRequirementsInput(
  exposureTimeMode: Nullable[ExposureTimeMode],
  minimumFov:   Nullable[Angle],
  narrowFilters: Nullable[Boolean],
  broadFilters:  Nullable[Boolean],
  gmosNorth:    Nullable[ImagingGmosNorthScienceRequirementsInput],
  gmosSouth:    Nullable[ImagingGmosSouthScienceRequirementsInput]
) {
  val scienceMode: ScienceMode = ScienceMode.Imaging
}

object ImagingScienceRequirementsInput:

  val Default: ImagingScienceRequirementsInput =
    ImagingScienceRequirementsInput(
      exposureTimeMode = Nullable.Null,
      minimumFov       = Nullable.Null,
      narrowFilters    = Nullable.Null,
      broadFilters     = Nullable.Null,
      gmosNorth        = Nullable.Null,
      gmosSouth        = Nullable.Null
    )

  val Binding: Matcher[ImagingScienceRequirementsInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ExposureTimeModeInput.Binding.Nullable("exposureTimeMode", rExposureTimeMode),
        AngleInput.Binding.Nullable("minimumFov", rMinimumFov),
        BooleanBinding.Nullable("narrowFilters", rNarrowFilter),
        BooleanBinding.Nullable("broadFilters", rBroadFilter),
        ImagingGmosNorthScienceRequirementsInput.Binding.Nullable("gmosNorth", rGmosNorth),
        ImagingGmosSouthScienceRequirementsInput.Binding.Nullable("gmosSouth", rGmosSouth)
      ) =>
        (rExposureTimeMode, rMinimumFov, rNarrowFilter, rBroadFilter, rGmosNorth, rGmosSouth)
          .parMapN(ImagingScienceRequirementsInput.apply)
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
