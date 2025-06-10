// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.functor.*
import cats.syntax.parallel.*
import lucuma.core.enums.ObservingModeType
import lucuma.odb.graphql.binding.*

object ObservingModeInput {

  final case class Create(
    gmosNorthLongSlit: Option[GmosLongSlitInput.Create.North],
    gmosSouthLongSlit: Option[GmosLongSlitInput.Create.South],
    flamingos2LongSlit: Option[Flamingos2LongSlitInput.Create],
    gmosNorthImaging: Option[GmosImagingInput.Create.North],
    gmosSouthImaging: Option[GmosImagingInput.Create.South]
  ) {

    def observingModeType: Option[ObservingModeType] =
      gmosNorthLongSlit.map(_.observingModeType)
        .orElse(gmosSouthLongSlit.map(_.observingModeType))
        .orElse(flamingos2LongSlit.map(_.observingModeType))
        .orElse(gmosNorthImaging.map(_.observingModeType))
        .orElse(gmosSouthImaging.map(_.observingModeType))

  }

  object Create {

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          GmosLongSlitInput.Create.North.Binding.Option("gmosNorthLongSlit", rGmosNorthLongSlit),
          GmosLongSlitInput.Create.South.Binding.Option("gmosSouthLongSlit", rGmosSouthLongSlit),
          Flamingos2LongSlitInput.Create.Binding.Option("flamingos2LongSlit", rFlamingos2LongSlit),
          GmosImagingInput.Create.North.Binding.Option("gmosNorthImaging", rGmosNorthImaging),
          GmosImagingInput.Create.South.Binding.Option("gmosSouthImaging", rGmosSouthImaging)
        ) =>
          (rGmosNorthLongSlit, rGmosSouthLongSlit, rFlamingos2LongSlit, rGmosNorthImaging, rGmosSouthImaging).parTupled.flatMap {
            case (gmosNorthLongSlit, gmosSouthLongSlit, flamingos2LongSlit, gmosNorthImaging, gmosSouthImaging) =>
              oneOrFail(
                gmosNorthLongSlit  -> "gmosNorthLongSlit",
                gmosSouthLongSlit  -> "gmosSouthLongSlit",
                flamingos2LongSlit -> "flamingos2LongSlit",
                gmosNorthImaging   -> "gmosNorthImaging",
                gmosSouthImaging   -> "gmosSouthImaging"
              ).as(Create(gmosNorthLongSlit, gmosSouthLongSlit, flamingos2LongSlit, gmosNorthImaging, gmosSouthImaging))

          }
      }

  }

  final case class Edit(
    gmosNorthLongSlit: Option[GmosLongSlitInput.Edit.North],
    gmosSouthLongSlit: Option[GmosLongSlitInput.Edit.South],
    flamingos2LongSlit: Option[Flamingos2LongSlitInput.Edit],
    gmosNorthImaging: Option[GmosImagingInput.Edit.North],
    gmosSouthImaging: Option[GmosImagingInput.Edit.South]
  ) {

    def observingModeType: Option[ObservingModeType] =
      gmosNorthLongSlit.map(_.observingModeType)
        .orElse(gmosSouthLongSlit.map(_.observingModeType))
        .orElse(flamingos2LongSlit.map(_.observingModeType))
        .orElse(gmosNorthImaging.flatMap(_.toCreate.toOption).map(_.observingModeType))
        .orElse(gmosSouthImaging.flatMap(_.toCreate.toOption).map(_.observingModeType))

  }

  object Edit {

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          GmosLongSlitInput.Edit.North.Binding.Option("gmosNorthLongSlit", rGmosNorthLongSlit),
          GmosLongSlitInput.Edit.South.Binding.Option("gmosSouthLongSlit", rGmosSouthLongSlit),
          Flamingos2LongSlitInput.Edit.Binding.Option("flamingos2LongSlit", rFlamingos2LongSlit),
          GmosImagingInput.Edit.North.Binding.Option("gmosNorthImaging", rGmosNorthImaging),
          GmosImagingInput.Edit.South.Binding.Option("gmosSouthImaging", rGmosSouthImaging)
        ) =>
          (rGmosNorthLongSlit, rGmosSouthLongSlit, rFlamingos2LongSlit, rGmosNorthImaging, rGmosSouthImaging).parTupled.flatMap {
            case (gmosNorthLongSlit, gmosSouthLongSlit, flamingos2LongSlit, gmosNorthImaging, gmosSouthImaging) =>
              oneOrFail(
                gmosNorthLongSlit  -> "gmosNorthLongSlit",
                gmosSouthLongSlit  -> "gmosSouthLongSlit",
                flamingos2LongSlit -> "flamingos2LongSlit",
                gmosNorthImaging   -> "gmosNorthImaging",
                gmosSouthImaging   -> "gmosSouthImaging"
              ).as(Edit(gmosNorthLongSlit, gmosSouthLongSlit, flamingos2LongSlit, gmosNorthImaging, gmosSouthImaging))
          }
      }

  }
}
