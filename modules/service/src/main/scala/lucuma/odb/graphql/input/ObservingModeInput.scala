// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.functor.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import grackle.Result
import lucuma.core.enums.ObservingModeType
import lucuma.odb.graphql.binding.*

object ObservingModeInput {

  final case class Create(
    gmosNorthLongSlit: Option[GmosLongSlitInput.Create.North],
    gmosSouthLongSlit: Option[GmosLongSlitInput.Create.South],
    gmosNorthImaging: Option[GmosImagingInput.Create.North],
    gmosSouthImaging: Option[GmosImagingInput.Create.South],
    flamingos2LongSlit: Option[Flamingos2LongSlitInput.Create]
  ) {

    def observingModeType: Option[ObservingModeType] =
      gmosNorthLongSlit.map(_.observingModeType)
        .orElse(gmosSouthLongSlit.map(_.observingModeType))
        .orElse(gmosNorthImaging.map(_.observingModeType))
        .orElse(gmosSouthImaging.map(_.observingModeType))
        .orElse(flamingos2LongSlit.map(_.observingModeType))

  }

  object Create {

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          GmosLongSlitInput.Create.North.Binding.Option("gmosNorthLongSlit", rGmosNorthLongSlit),
          GmosLongSlitInput.Create.South.Binding.Option("gmosSouthLongSlit", rGmosSouthLongSlit),
          GmosImagingInput.Create.North.Binding.Option("gmosNorthImaging", rGmosNorthImaging),
          GmosImagingInput.Create.South.Binding.Option("gmosSouthImaging", rGmosSouthImaging),
          Flamingos2LongSlitInput.Create.Binding.Option("flamingos2LongSlit", rFlamingos2LongSlit)
        ) =>
          (rGmosNorthLongSlit, rGmosSouthLongSlit, rGmosNorthImaging, rGmosSouthImaging, rFlamingos2LongSlit).parTupled.flatMap {
            case (gmosNorthLongSlit, gmosSouthLongSlit, gmosNorthImaging, gmosSouthImaging, flamingos2LongSlit) =>
              oneOrFail(
                gmosNorthLongSlit  -> "gmosNorthLongSlit",
                gmosSouthLongSlit  -> "gmosSouthLongSlit",
                gmosNorthImaging   -> "gmosNorthImaging",
                gmosSouthImaging   -> "gmosSouthImaging",
                flamingos2LongSlit -> "flamingos2LongSlit"
              ).as(Create(gmosNorthLongSlit, gmosSouthLongSlit, gmosNorthImaging, gmosSouthImaging, flamingos2LongSlit))

          }
      }

  }

  final case class Edit(
    gmosNorthLongSlit: Option[GmosLongSlitInput.Edit.North],
    gmosSouthLongSlit: Option[GmosLongSlitInput.Edit.South],
    gmosNorthImaging: Option[GmosImagingInput.Edit.North],
    gmosSouthImaging: Option[GmosImagingInput.Edit.South],
    flamingos2LongSlit: Option[Flamingos2LongSlitInput.Edit]
  ) {

    def observingModeType: Option[ObservingModeType] =
      gmosNorthLongSlit.map(_.observingModeType)
        .orElse(gmosSouthLongSlit.map(_.observingModeType))
        .orElse(gmosNorthImaging.map(_.observingModeType))
        .orElse(gmosSouthImaging.map(_.observingModeType))
        .orElse(flamingos2LongSlit.map(_.observingModeType))

    def toCreate: Result[Create] =
      (gmosNorthLongSlit.traverse(_.toCreate),
       gmosSouthLongSlit.traverse(_.toCreate),
       gmosNorthImaging.traverse(_.toCreate),
       gmosSouthImaging.traverse(_.toCreate),
       flamingos2LongSlit.traverse(_.toCreate)
      ).parMapN(Create.apply)

  }

  object Edit {

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          GmosLongSlitInput.Edit.North.Binding.Option("gmosNorthLongSlit", rGmosNorthLongSlit),
          GmosLongSlitInput.Edit.South.Binding.Option("gmosSouthLongSlit", rGmosSouthLongSlit),
          GmosImagingInput.Edit.North.Binding.Option("gmosNorthImaging", rGmosNorthImaging),
          GmosImagingInput.Edit.South.Binding.Option("gmosSouthImaging", rGmosSouthImaging),
          Flamingos2LongSlitInput.Edit.Binding.Option("flamingos2LongSlit", rFlamingos2LongSlit)
        ) =>
          (rGmosNorthLongSlit, rGmosSouthLongSlit, rGmosNorthImaging, rGmosSouthImaging, rFlamingos2LongSlit).parTupled.flatMap {
            case (gmosNorthLongSlit, gmosSouthLongSlit, gmosNorthImaging, gmosSouthImaging, flamingos2LongSlit) =>
              oneOrFail(
                gmosNorthLongSlit  -> "gmosNorthLongSlit",
                gmosSouthLongSlit  -> "gmosSouthLongSlit",
                gmosNorthImaging   -> "gmosNorthImaging",
                gmosSouthImaging   -> "gmosSouthImaging",
                flamingos2LongSlit -> "flamingos2LongSlit"
              ).as(Edit(gmosNorthLongSlit, gmosSouthLongSlit, gmosNorthImaging, gmosSouthImaging, flamingos2LongSlit))
          }
      }

  }
}
