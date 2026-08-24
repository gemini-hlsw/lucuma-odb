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
    exchange:           Option[ExchangeInput.Create],
    flamingos2Imaging:  Option[Flamingos2ImagingInput.Create],
    flamingos2LongSlit: Option[Flamingos2LongSlitInput.Create],
    flamingos2Mos:      Option[Flamingos2MosInput.Create],
    ghostIfu:           Option[GhostIfuInput.Create],
    gmosNorthImaging:   Option[GmosImagingInput.Create.North],
    gmosNorthLongSlit:  Option[GmosLongSlitInput.Create.North],
    gmosNorthMos:       Option[GmosMosInput.Create.North],
    gmosSouthImaging:   Option[GmosImagingInput.Create.South],
    gmosSouthLongSlit:  Option[GmosLongSlitInput.Create.South],
    gmosSouthMos:       Option[GmosMosInput.Create.South],
    gnirsImaging:       Option[GnirsImagingInput.Create],
    gnirsSpectroscopy:  Option[GnirsSpectroscopyInput.Create],
    igrins2LongSlit:    Option[Igrins2LongSlitInput.Create],
    visitor:            Option[VisitorInput.Create]
  ):

    def observingModeType: Option[ObservingModeType] =
      gmosNorthLongSlit
        .map(_.observingModeType)
        .orElse(exchange.map(_.mode))
        .orElse(flamingos2Imaging.map(_.observingModeType))
        .orElse(flamingos2LongSlit.map(_.observingModeType))
        .orElse(flamingos2Mos.map(_.observingModeType))
        .orElse(ghostIfu.map(_.observingModeType))
        .orElse(gmosNorthImaging.as(ObservingModeType.GmosNorthImaging))
        .orElse(gmosNorthLongSlit.map(_.observingModeType))
        .orElse(gmosNorthMos.map(_.observingModeType))
        .orElse(gmosSouthImaging.as(ObservingModeType.GmosSouthImaging))
        .orElse(gmosSouthLongSlit.map(_.observingModeType))
        .orElse(gmosSouthMos.map(_.observingModeType))
        .orElse(gnirsImaging.map(_.observingModeType))
        .orElse(gnirsSpectroscopy.map(_.observingModeType))
        .orElse(igrins2LongSlit.map(_.observingModeType))
        .orElse(visitor.map(_.mode))

    def needsStaffAccess: Boolean =
      gnirsSpectroscopy.exists(_.needsStaffAccess)

  object Create:

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          ExchangeInput.CreateBinding.Option("exchange", rExchange),
          Flamingos2ImagingInput.Create.Binding.Option("flamingos2Imaging", rFlamingos2Imaging),
          Flamingos2LongSlitInput.Create.Binding.Option("flamingos2LongSlit", rFlamingos2LongSlit),
          Flamingos2MosInput.Create.Binding.Option("flamingos2Mos", rFlamingos2Mos),
          GhostIfuInput.Create.Binding.Option("ghostIfu", rGhostIfu),
          GmosImagingInput.Create.NorthBinding.Option("gmosNorthImaging", rGmosNorthImaging),
          GmosLongSlitInput.Create.North.Binding.Option("gmosNorthLongSlit", rGmosNorthLongSlit),
          GmosMosInput.Create.North.Binding.Option("gmosNorthMos", rGmosNorthMos),
          GmosImagingInput.Create.SouthBinding.Option("gmosSouthImaging", rGmosSouthImaging),
          GmosLongSlitInput.Create.South.Binding.Option("gmosSouthLongSlit", rGmosSouthLongSlit),
          GmosMosInput.Create.South.Binding.Option("gmosSouthMos", rGmosSouthMos),
          GnirsIfuInput.Create.Binding.Option("gnirsIfu", rGnirsIfu),
          GnirsImagingInput.Create.Binding.Option("gnirsImaging", rGnirsImaging),
          GnirsLongSlitInput.Create.Binding.Option("gnirsLongSlit", rGnirsLongSlit),
          GnirsSpectroscopyInput.Create.Binding.Option("gnirsSpectroscopy", rGnirsSpectroscopy),
          Igrins2LongSlitInput.Create.Binding.Option("igrins2LongSlit", rIgrins2LongSlit),
          VisitorInput.CreateBinding.Option("visitor", rVisitor)
        ) =>
          (rExchange,
           rFlamingos2Imaging,
           rFlamingos2LongSlit,
           rFlamingos2Mos,
           rGhostIfu,
           rGmosNorthImaging,
           rGmosNorthLongSlit,
           rGmosNorthMos,
           rGmosSouthImaging,
           rGmosSouthLongSlit,
           rGmosSouthMos,
           rGnirsIfu,
           rGnirsImaging,
           rGnirsLongSlit,
           rGnirsSpectroscopy,
           rIgrins2LongSlit,
           rVisitor
          ).parTupled.flatMap:
            case (exchange, flamingos2Imaging, flamingos2LongSlit, flamingos2Mos, ghostIfu, gmosNorthImaging, gmosNorthLongSlit, gmosNorthMos, gmosSouthImaging, gmosSouthLongSlit, gmosSouthMos, gnirsIfu, gnirsImaging, gnirsLongSlit, gnirsSpectroscopy, igrins2LongSlit, visitor) =>
              oneOrFail(
                exchange           -> "exchange",
                flamingos2Imaging  -> "flamingos2Imaging",
                flamingos2LongSlit -> "flamingos2LongSlit",
                flamingos2Mos      -> "flamingos2Mos",
                ghostIfu           -> "ghostIfu",
                gmosNorthImaging   -> "gmosNorthImaging",
                gmosNorthLongSlit  -> "gmosNorthLongSlit",
                gmosNorthMos       -> "gmosNorthMos",
                gmosSouthImaging   -> "gmosSouthImaging",
                gmosSouthLongSlit  -> "gmosSouthLongSlit",
                gmosSouthMos       -> "gmosSouthMos",
                gnirsIfu           -> "gnirsIfu",
                gnirsImaging       -> "gnirsImaging",
                gnirsLongSlit      -> "gnirsLongSlit",
                gnirsSpectroscopy  -> "gnirsSpectroscopy",
                igrins2LongSlit    -> "igrins2LongSlit",
                visitor            -> "visitor"
              ).as(Create(exchange, flamingos2Imaging, flamingos2LongSlit, flamingos2Mos, ghostIfu, gmosNorthImaging, gmosNorthLongSlit, gmosNorthMos, gmosSouthImaging, gmosSouthLongSlit, gmosSouthMos, gnirsImaging, gnirsSpectroscopy.orElse(gnirsLongSlit).orElse(gnirsIfu), igrins2LongSlit, visitor))

  final case class Edit(
    exchange:           Option[ExchangeInput.Edit],
    flamingos2Imaging:  Option[Flamingos2ImagingInput.Edit],
    flamingos2LongSlit: Option[Flamingos2LongSlitInput.Edit],
    flamingos2Mos:      Option[Flamingos2MosInput.Edit],
    ghostIfu:           Option[GhostIfuInput.Edit],
    gmosNorthImaging:   Option[GmosImagingInput.Edit.North],
    gmosNorthLongSlit:  Option[GmosLongSlitInput.Edit.North],
    gmosNorthMos:       Option[GmosMosInput.Edit.North],
    gmosSouthImaging:   Option[GmosImagingInput.Edit.South],
    gmosSouthLongSlit:  Option[GmosLongSlitInput.Edit.South],
    gmosSouthMos:       Option[GmosMosInput.Edit.South],
    gnirsImaging:       Option[GnirsImagingInput.Edit],
    gnirsSpectroscopy:  Option[GnirsSpectroscopyInput.Edit],
    igrins2LongSlit:    Option[Igrins2LongSlitInput.Edit],
    visitor:            Option[VisitorInput.Edit]
  ):
    def updatesAcquisition: Boolean =
      flamingos2LongSlit.exists(_.updatesAcquisition) ||
      flamingos2Mos.exists(_.updatesAcquisition)      ||
      gmosNorthLongSlit.exists(_.updatesAcquisition)  ||
      gmosSouthLongSlit.exists(_.updatesAcquisition)  ||
      gnirsSpectroscopy.exists(_.updatesAcquisition)

    def limitToPreExecution(access: Access): Boolean =
      access <= Access.Pi                                        ||
        flamingos2Imaging.isDefined                              ||
        flamingos2LongSlit.exists(_.limitToPreExecution(access)) ||
        flamingos2Mos.exists(_.limitToPreExecution(access))      ||
        ghostIfu.isDefined                                       ||
        gmosNorthImaging.isDefined                               ||
        gmosNorthLongSlit.exists(_.limitToPreExecution(access))  ||
        gmosSouthImaging.isDefined                               ||
        gmosSouthLongSlit.exists(_.limitToPreExecution(access))  ||
        gmosNorthMos.isDefined                                   ||
        gmosSouthMos.isDefined                                   ||
        gnirsImaging.isDefined                                   ||
        gnirsSpectroscopy.isDefined                              ||
        igrins2LongSlit.isDefined

    def needsStaffAccess: Boolean =
      gnirsSpectroscopy.exists(_.needsStaffAccess)

    def observingModeType: Option[ObservingModeType] =
      exchange.flatMap(_.mode)
        .orElse(flamingos2Imaging.map(_.observingModeType))
        .orElse(flamingos2LongSlit.map(_.observingModeType))
        .orElse(flamingos2Mos.map(_.observingModeType))
        .orElse(ghostIfu.map(_.observingModeType))
        .orElse(gmosNorthImaging.as(ObservingModeType.GmosNorthImaging))
        .orElse(gmosNorthLongSlit.map(_.observingModeType))
        .orElse(gmosNorthMos.map(_.observingModeType))
        .orElse(gmosSouthImaging.as(ObservingModeType.GmosSouthImaging))
        .orElse(gmosSouthLongSlit.map(_.observingModeType))
        .orElse(gmosSouthMos.map(_.observingModeType))
        .orElse(gnirsImaging.map(_.observingModeType))
        .orElse(gnirsSpectroscopy.flatMap(_.observingModeType))
        .orElse(igrins2LongSlit.map(_.observingModeType))
        .orElse(visitor.flatMap(_.mode))

    def toCreate: Result[Create] =
      (exchange.traverse(_.toCreate),
       flamingos2Imaging.traverse(_.toCreate),
       flamingos2LongSlit.traverse(_.toCreate),
       flamingos2Mos.traverse(_.toCreate),
       ghostIfu.traverse(_.toCreate),
       gmosNorthImaging.traverse(_.toCreate),
       gmosNorthLongSlit.traverse(_.toCreate),
       gmosNorthMos.traverse(_.toCreate),
       gmosSouthImaging.traverse(_.toCreate),
       gmosSouthLongSlit.traverse(_.toCreate),
       gmosSouthMos.traverse(_.toCreate),
       gnirsImaging.traverse(_.toCreate),
       gnirsSpectroscopy.traverse(_.toCreate),
       igrins2LongSlit.traverse(_.toCreate),
       visitor.traverse(_.toCreate)
      ).parMapN(Create.apply)

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          ExchangeInput.EditBinding.Option("exchange", rExchange),
          Flamingos2ImagingInput.Edit.Binding.Option("flamingos2Imaging", rFlamingos2Imaging),
          Flamingos2LongSlitInput.Edit.Binding.Option("flamingos2LongSlit", rFlamingos2LongSlit),
          Flamingos2MosInput.Edit.Binding.Option("flamingos2Mos", rFlamingos2Mos),
          GhostIfuInput.Edit.Binding.Option("ghostIfu", rGhostIfu),
          GmosImagingInput.Edit.NorthBinding.Option("gmosNorthImaging", rGmosNorthImaging),
          GmosLongSlitInput.Edit.North.Binding.Option("gmosNorthLongSlit", rGmosNorthLongSlit),
          GmosMosInput.Edit.North.Binding.Option("gmosNorthMos", rGmosNorthMos),
          GmosImagingInput.Edit.SouthBinding.Option("gmosSouthImaging", rGmosSouthImaging),
          GmosLongSlitInput.Edit.South.Binding.Option("gmosSouthLongSlit", rGmosSouthLongSlit),
          GmosMosInput.Edit.South.Binding.Option("gmosSouthMos", rGmosSouthMos),
          GnirsIfuInput.Edit.Binding.Option("gnirsIfu", rGnirsIfu),
          GnirsImagingInput.Edit.Binding.Option("gnirsImaging", rGnirsImaging),
          GnirsLongSlitInput.Edit.Binding.Option("gnirsLongSlit", rGnirsLongSlit),
          GnirsSpectroscopyInput.Edit.Binding.Option("gnirsSpectroscopy", rGnirsSpectroscopy),
          Igrins2LongSlitInput.Edit.Binding.Option("igrins2LongSlit", rIgrins2LongSlit),
          VisitorInput.EditBinding.Option("visitor", rVisitor),
        ) =>
          (rExchange,
           rFlamingos2Imaging,
           rFlamingos2LongSlit,
           rFlamingos2Mos,
           rGhostIfu,
           rGmosNorthImaging,
           rGmosNorthLongSlit,
           rGmosNorthMos,
           rGmosSouthImaging,
           rGmosSouthLongSlit,
           rGmosSouthMos,
           rGnirsIfu,
           rGnirsImaging,
           rGnirsLongSlit,
           rGnirsSpectroscopy,
           rIgrins2LongSlit,
           rVisitor,
          ).parTupled.flatMap:
            case (exchange, flamingos2Imaging, flamingos2LongSlit, flamingos2Mos, ghostIfu, gmosNorthImaging, gmosNorthLongSlit, gmosNorthMos, gmosSouthImaging, gmosSouthLongSlit, gmosSouthMos, gnirsIfu, gnirsImaging, gnirsLongSlit, gnirsSpectroscopy, igrins2LongSlit, visitor) =>
              oneOrFail(
                exchange           -> "exchange",
                flamingos2Imaging  -> "flamingos2Imaging",
                flamingos2LongSlit -> "flamingos2LongSlit",
                flamingos2Mos      -> "flamingos2Mos",
                ghostIfu           -> "ghostIfu",
                gmosNorthImaging   -> "gmosNorthImaging",
                gmosNorthLongSlit  -> "gmosNorthLongSlit",
                gmosNorthMos       -> "gmosNorthMos",
                gmosSouthImaging   -> "gmosSouthImaging",
                gmosSouthLongSlit  -> "gmosSouthLongSlit",
                gmosSouthMos       -> "gmosSouthMos",
                gnirsIfu           -> "gnirsIfu",
                gnirsImaging       -> "gnirsImaging",
                gnirsLongSlit      -> "gnirsLongSlit",
                gnirsSpectroscopy  -> "gnirsSpectroscopy",
                igrins2LongSlit    -> "igrins2LongSlit",
                visitor            -> "visitor"
              ).as(Edit(exchange, flamingos2Imaging, flamingos2LongSlit, flamingos2Mos, ghostIfu, gmosNorthImaging, gmosNorthLongSlit, gmosNorthMos, gmosSouthImaging, gmosSouthLongSlit, gmosSouthMos, gnirsImaging, gnirsSpectroscopy.orElse(gnirsLongSlit).orElse(gnirsIfu), igrins2LongSlit, visitor))
