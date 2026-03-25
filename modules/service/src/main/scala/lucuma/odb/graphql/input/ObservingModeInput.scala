// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.functor.*
import cats.syntax.parallel.*
import cats.syntax.partialOrder.*
import cats.syntax.traverse.*
import grackle.Result
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Access
import lucuma.odb.graphql.binding.*

object ObservingModeInput:

  final case class Create(
    gmosNorthLongSlit: Option[GmosLongSlitInput.Create.North],
    gmosSouthLongSlit: Option[GmosLongSlitInput.Create.South],
    gmosNorthImaging: Option[GmosImagingInput.Create.North],
    gmosSouthImaging: Option[GmosImagingInput.Create.South],
    flamingos2LongSlit: Option[Flamingos2LongSlitInput.Create],
    igrins2LongSlit: Option[Igrins2LongSlitInput.Create]
  ):

    def observingModeType: Option[ObservingModeType] =
      gmosNorthLongSlit
        .map(_.observingModeType)
        .orElse(gmosSouthLongSlit.map(_.observingModeType))
        .orElse(gmosNorthImaging.as(ObservingModeType.GmosNorthImaging))
        .orElse(gmosSouthImaging.as(ObservingModeType.GmosSouthImaging))
        .orElse(flamingos2LongSlit.map(_.observingModeType))
        .orElse(igrins2LongSlit.map(_.observingModeType))

  object Create:

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          GmosLongSlitInput.Create.North.Binding.Option("gmosNorthLongSlit", rGmosNorthLongSlit),
          GmosLongSlitInput.Create.South.Binding.Option("gmosSouthLongSlit", rGmosSouthLongSlit),
          GmosImagingInput.Create.NorthBinding.Option("gmosNorthImaging", rGmosNorthImaging),
          GmosImagingInput.Create.SouthBinding.Option("gmosSouthImaging", rGmosSouthImaging),
          Flamingos2LongSlitInput.Create.Binding.Option("flamingos2LongSlit", rFlamingos2LongSlit),
          Igrins2LongSlitInput.Create.Binding.Option("igrins2LongSlit", rIgrins2LongSlit)
        ) =>
          (rGmosNorthLongSlit, rGmosSouthLongSlit, rGmosNorthImaging, rGmosSouthImaging, rFlamingos2LongSlit, rIgrins2LongSlit).parTupled.flatMap {
            case (gmosNorthLongSlit, gmosSouthLongSlit, gmosNorthImaging, gmosSouthImaging, flamingos2LongSlit, igrins2LongSlit) =>
              oneOrFail(
                gmosNorthLongSlit  -> "gmosNorthLongSlit",
                gmosSouthLongSlit  -> "gmosSouthLongSlit",
                gmosNorthImaging   -> "gmosNorthImaging",
                gmosSouthImaging   -> "gmosSouthImaging",
                flamingos2LongSlit -> "flamingos2LongSlit",
                igrins2LongSlit    -> "igrins2LongSlit"
              ).as(Create(gmosNorthLongSlit, gmosSouthLongSlit, gmosNorthImaging, gmosSouthImaging, flamingos2LongSlit, igrins2LongSlit))

          }
      }

  final case class Edit(
    gmosNorthLongSlit: Option[GmosLongSlitInput.Edit.North],
    gmosSouthLongSlit: Option[GmosLongSlitInput.Edit.South],
    gmosNorthImaging: Option[GmosImagingInput.Edit.North],
    gmosSouthImaging: Option[GmosImagingInput.Edit.South],
    flamingos2LongSlit: Option[Flamingos2LongSlitInput.Edit],
    igrins2LongSlit: Option[Igrins2LongSlitInput.Edit]
  ):
    def updatesAcquisition: Boolean =
      flamingos2LongSlit.exists(_.updatesAcquisition) ||
      gmosNorthLongSlit.exists(_.updatesAcquisition)  ||
      gmosSouthLongSlit.exists(_.updatesAcquisition)

    def limitToPreExecution(access: Access): Boolean =
      access <= Access.Pi ||
        gmosNorthLongSlit.exists(_.limitToPreExecution(access)) ||
        gmosSouthLongSlit.exists(_.limitToPreExecution(access)) ||
        gmosNorthImaging.isDefined ||
        gmosSouthImaging.isDefined ||
        flamingos2LongSlit.exists(_.limitToPreExecution(access)) ||
        igrins2LongSlit.isDefined

    def observingModeType: Option[ObservingModeType] =
      gmosNorthLongSlit.map(_.observingModeType)
        .orElse(gmosSouthLongSlit.map(_.observingModeType))
        .orElse(gmosNorthImaging.as(ObservingModeType.GmosNorthImaging))
        .orElse(gmosSouthImaging.as(ObservingModeType.GmosSouthImaging))
        .orElse(flamingos2LongSlit.map(_.observingModeType))
        .orElse(igrins2LongSlit.map(_.observingModeType))

    def toCreate: Result[Create] =
      (gmosNorthLongSlit.traverse(_.toCreate),
       gmosSouthLongSlit.traverse(_.toCreate),
       gmosNorthImaging.traverse(_.toCreate),
       gmosSouthImaging.traverse(_.toCreate),
       flamingos2LongSlit.traverse(_.toCreate),
       igrins2LongSlit.traverse(_.toCreate)
      ).parMapN(Create.apply)

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          GmosLongSlitInput.Edit.North.Binding.Option("gmosNorthLongSlit", rGmosNorthLongSlit),
          GmosLongSlitInput.Edit.South.Binding.Option("gmosSouthLongSlit", rGmosSouthLongSlit),
          GmosImagingInput.Edit.NorthBinding.Option("gmosNorthImaging", rGmosNorthImaging),
          GmosImagingInput.Edit.SouthBinding.Option("gmosSouthImaging", rGmosSouthImaging),
          Flamingos2LongSlitInput.Edit.Binding.Option("flamingos2LongSlit", rFlamingos2LongSlit),
          Igrins2LongSlitInput.Edit.Binding.Option("igrins2LongSlit", rIgrins2LongSlit)
        ) =>
          (rGmosNorthLongSlit, rGmosSouthLongSlit, rGmosNorthImaging, rGmosSouthImaging, rFlamingos2LongSlit, rIgrins2LongSlit).parTupled.flatMap {
            case (gmosNorthLongSlit, gmosSouthLongSlit, gmosNorthImaging, gmosSouthImaging, flamingos2LongSlit, igrins2LongSlit) =>
              oneOrFail(
                gmosNorthLongSlit  -> "gmosNorthLongSlit",
                gmosSouthLongSlit  -> "gmosSouthLongSlit",
                gmosNorthImaging   -> "gmosNorthImaging",
                gmosSouthImaging   -> "gmosSouthImaging",
                flamingos2LongSlit -> "flamingos2LongSlit",
                igrins2LongSlit    -> "igrins2LongSlit"
              ).as(Edit(gmosNorthLongSlit, gmosSouthLongSlit, gmosNorthImaging, gmosSouthImaging, flamingos2LongSlit, igrins2LongSlit))
          }
      }

