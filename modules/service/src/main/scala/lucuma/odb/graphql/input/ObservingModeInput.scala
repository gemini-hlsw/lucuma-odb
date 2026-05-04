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
    flamingos2LongSlit: Option[Flamingos2LongSlitInput.Create],
    ghostIfu:           Option[GhostIfuInput.Create],
    gmosNorthImaging:   Option[GmosImagingInput.Create.North],
    gmosNorthLongSlit:  Option[GmosLongSlitInput.Create.North],
    gmosSouthImaging:   Option[GmosImagingInput.Create.South],
    gmosSouthLongSlit:  Option[GmosLongSlitInput.Create.South],
    igrins2LongSlit:    Option[Igrins2LongSlitInput.Create],
    visitor:            Option[VisitorInput.Create]
  ):

    def observingModeType: Option[ObservingModeType] =
      gmosNorthLongSlit
        .map(_.observingModeType)
        .orElse(flamingos2LongSlit.map(_.observingModeType))
        .orElse(ghostIfu.map(_.observingModeType))
        .orElse(gmosNorthImaging.as(ObservingModeType.GmosNorthImaging))
        .orElse(gmosNorthLongSlit.map(_.observingModeType))
        .orElse(gmosSouthImaging.as(ObservingModeType.GmosSouthImaging))
        .orElse(gmosSouthLongSlit.map(_.observingModeType))
        .orElse(igrins2LongSlit.map(_.observingModeType))
        .orElse(visitor.map(_.mode))

  object Create:

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          Flamingos2LongSlitInput.Create.Binding.Option("flamingos2LongSlit", rFlamingos2LongSlit),
          GhostIfuInput.Create.Binding.Option("ghostIfu", rGhostIfu),
          GmosImagingInput.Create.NorthBinding.Option("gmosNorthImaging", rGmosNorthImaging),
          GmosLongSlitInput.Create.North.Binding.Option("gmosNorthLongSlit", rGmosNorthLongSlit),
          GmosImagingInput.Create.SouthBinding.Option("gmosSouthImaging", rGmosSouthImaging),
          GmosLongSlitInput.Create.South.Binding.Option("gmosSouthLongSlit", rGmosSouthLongSlit),
          Igrins2LongSlitInput.Create.Binding.Option("igrins2LongSlit", rIgrins2LongSlit),
          VisitorInput.CreateBinding.Option("visitor", rVisitor)
        ) =>
          (rFlamingos2LongSlit,
           rGhostIfu,
           rGmosNorthImaging,
           rGmosNorthLongSlit,
           rGmosSouthImaging,
           rGmosSouthLongSlit,
           rIgrins2LongSlit,
           rVisitor
          ).parTupled.flatMap:
            case (flamingos2LongSlit, ghostIfu, gmosNorthImaging, gmosNorthLongSlit, gmosSouthImaging, gmosSouthLongSlit, igrins2LongSlit, visitor) =>
              oneOrFail(
                flamingos2LongSlit -> "flamingos2LongSlit",
                ghostIfu           -> "ghostIfu",
                gmosNorthImaging   -> "gmosNorthImaging",
                gmosNorthLongSlit  -> "gmosNorthLongSlit",
                gmosSouthImaging   -> "gmosSouthImaging",
                gmosSouthLongSlit  -> "gmosSouthLongSlit",
                igrins2LongSlit    -> "igrins2LongSlit",
                visitor            -> "visitor"
              ).as(Create(flamingos2LongSlit, ghostIfu, gmosNorthImaging, gmosNorthLongSlit, gmosSouthImaging, gmosSouthLongSlit, igrins2LongSlit, visitor))

  final case class Edit(
    flamingos2LongSlit: Option[Flamingos2LongSlitInput.Edit],
    ghostIfu:           Option[GhostIfuInput.Edit],
    gmosNorthImaging:   Option[GmosImagingInput.Edit.North],
    gmosNorthLongSlit:  Option[GmosLongSlitInput.Edit.North],
    gmosSouthImaging:   Option[GmosImagingInput.Edit.South],
    gmosSouthLongSlit:  Option[GmosLongSlitInput.Edit.South],
    igrins2LongSlit:    Option[Igrins2LongSlitInput.Edit],
    visitor:            Option[VisitorInput.Edit]
  ):
    def updatesAcquisition: Boolean =
      flamingos2LongSlit.exists(_.updatesAcquisition) ||
      gmosNorthLongSlit.exists(_.updatesAcquisition)  ||
      gmosSouthLongSlit.exists(_.updatesAcquisition)

    def limitToPreExecution(access: Access): Boolean =
      access <= Access.Pi                                        ||
        flamingos2LongSlit.exists(_.limitToPreExecution(access)) ||
        ghostIfu.isDefined                                       ||
        gmosNorthImaging.isDefined                               ||
        gmosNorthLongSlit.exists(_.limitToPreExecution(access))  ||
        gmosSouthImaging.isDefined                               ||
        gmosSouthLongSlit.exists(_.limitToPreExecution(access))  ||
        igrins2LongSlit.isDefined

    def observingModeType: Option[ObservingModeType] =
      flamingos2LongSlit.map(_.observingModeType)
        .orElse(ghostIfu.map(_.observingModeType))
        .orElse(gmosNorthImaging.as(ObservingModeType.GmosNorthImaging))
        .orElse(gmosNorthLongSlit.map(_.observingModeType))
        .orElse(gmosSouthImaging.as(ObservingModeType.GmosSouthImaging))
        .orElse(gmosSouthLongSlit.map(_.observingModeType))
        .orElse(igrins2LongSlit.map(_.observingModeType))
        .orElse(visitor.flatMap(_.mode))

    def toCreate: Result[Create] =
      (flamingos2LongSlit.traverse(_.toCreate),
       ghostIfu.traverse(_.toCreate),
       gmosNorthImaging.traverse(_.toCreate),
       gmosNorthLongSlit.traverse(_.toCreate),
       gmosSouthImaging.traverse(_.toCreate),
       gmosSouthLongSlit.traverse(_.toCreate),
       igrins2LongSlit.traverse(_.toCreate),
       visitor.traverse(_.toCreate)
      ).parMapN(Create.apply)

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          Flamingos2LongSlitInput.Edit.Binding.Option("flamingos2LongSlit", rFlamingos2LongSlit),
          GhostIfuInput.Edit.Binding.Option("ghostIfu", rGhostIfu),
          GmosImagingInput.Edit.NorthBinding.Option("gmosNorthImaging", rGmosNorthImaging),
          GmosLongSlitInput.Edit.North.Binding.Option("gmosNorthLongSlit", rGmosNorthLongSlit),
          GmosImagingInput.Edit.SouthBinding.Option("gmosSouthImaging", rGmosSouthImaging),
          GmosLongSlitInput.Edit.South.Binding.Option("gmosSouthLongSlit", rGmosSouthLongSlit),
          Igrins2LongSlitInput.Edit.Binding.Option("igrins2LongSlit", rIgrins2LongSlit),
          VisitorInput.EditBinding.Option("visitor", rVisitor),
        ) =>
          (rFlamingos2LongSlit,
           rGhostIfu,
           rGmosNorthImaging,
           rGmosNorthLongSlit,
           rGmosSouthImaging,
           rGmosSouthLongSlit,
           rIgrins2LongSlit,
           rVisitor,
          ).parTupled.flatMap:
            case (flamingos2LongSlit, ghostIfu, gmosNorthImaging, gmosNorthLongSlit, gmosSouthImaging, gmosSouthLongSlit, igrins2LongSlit, visitor) =>
              oneOrFail(
                flamingos2LongSlit -> "flamingos2LongSlit",
                ghostIfu           -> "ghostIfu",
                gmosNorthImaging   -> "gmosNorthImaging",
                gmosNorthLongSlit  -> "gmosNorthLongSlit",
                gmosSouthImaging   -> "gmosSouthImaging",
                gmosSouthLongSlit  -> "gmosSouthLongSlit",
                igrins2LongSlit    -> "igrins2LongSlit",
                visitor            -> "visitor"
              ).as(Edit(flamingos2LongSlit, ghostIfu, gmosNorthImaging, gmosNorthLongSlit, gmosSouthImaging, gmosSouthLongSlit, igrins2LongSlit, visitor))