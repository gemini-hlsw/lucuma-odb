// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
    gmosNorthIfu:       Option[GmosIfuInput.Create.North],
    gmosNorthImaging:   Option[GmosImagingInput.Create.North],
    gmosNorthLongSlit:  Option[GmosLongSlitInput.Create.North],
    gmosNorthMos:       Option[GmosMosInput.Create.North],
    gmosSouthIfu:       Option[GmosIfuInput.Create.South],
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
        .orElse(gmosNorthIfu.map(_.observingModeType))
        .orElse(gmosNorthImaging.as(ObservingModeType.GmosNorthImaging))
        .orElse(gmosNorthLongSlit.map(_.observingModeType))
        .orElse(gmosNorthMos.map(_.observingModeType))
        .orElse(gmosSouthIfu.map(_.observingModeType))
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

    /**
     * No mode selected.  Callers `copy` the one field they mean, which is safer than seventeen
     * positional `none`s where a misplaced `Some` typechecks.
     */
    val Empty: Create =
      Create(
        exchange           = None,
        flamingos2Imaging  = None,
        flamingos2LongSlit = None,
        flamingos2Mos      = None,
        ghostIfu           = None,
        gmosNorthIfu       = None,
        gmosNorthImaging   = None,
        gmosNorthLongSlit  = None,
        gmosNorthMos       = None,
        gmosSouthIfu       = None,
        gmosSouthImaging   = None,
        gmosSouthLongSlit  = None,
        gmosSouthMos       = None,
        gnirsImaging       = None,
        gnirsSpectroscopy  = None,
        igrins2LongSlit    = None,
        visitor            = None
      )

    val Binding: Matcher[Create] =
      OneOfBinding(
        "exchange"           -> ExchangeInput.CreateBinding.map(m => Empty.copy(exchange = Some(m))),
        "flamingos2Imaging"  -> Flamingos2ImagingInput.Create.Binding.map(m => Empty.copy(flamingos2Imaging = Some(m))),
        "flamingos2LongSlit" -> Flamingos2LongSlitInput.Create.Binding.map(m => Empty.copy(flamingos2LongSlit = Some(m))),
        "flamingos2Mos"      -> Flamingos2MosInput.Create.Binding.map(m => Empty.copy(flamingos2Mos = Some(m))),
        "ghostIfu"           -> GhostIfuInput.Create.Binding.map(m => Empty.copy(ghostIfu = Some(m))),
        "gmosNorthIfu"       -> GmosIfuInput.Create.North.Binding.map(m => Empty.copy(gmosNorthIfu = Some(m))),
        "gmosNorthImaging"   -> GmosImagingInput.Create.NorthBinding.map(m => Empty.copy(gmosNorthImaging = Some(m))),
        "gmosNorthLongSlit"  -> GmosLongSlitInput.Create.North.Binding.map(m => Empty.copy(gmosNorthLongSlit = Some(m))),
        "gmosNorthMos"       -> GmosMosInput.Create.North.Binding.map(m => Empty.copy(gmosNorthMos = Some(m))),
        "gmosSouthIfu"       -> GmosIfuInput.Create.South.Binding.map(m => Empty.copy(gmosSouthIfu = Some(m))),
        "gmosSouthImaging"   -> GmosImagingInput.Create.SouthBinding.map(m => Empty.copy(gmosSouthImaging = Some(m))),
        "gmosSouthLongSlit"  -> GmosLongSlitInput.Create.South.Binding.map(m => Empty.copy(gmosSouthLongSlit = Some(m))),
        "gmosSouthMos"       -> GmosMosInput.Create.South.Binding.map(m => Empty.copy(gmosSouthMos = Some(m))),
        "gnirsIfu"           -> GnirsIfuInput.Create.Binding.map(m => Empty.copy(gnirsSpectroscopy = Some(m))),
        "gnirsImaging"       -> GnirsImagingInput.Create.Binding.map(m => Empty.copy(gnirsImaging = Some(m))),
        "gnirsLongSlit"      -> GnirsLongSlitInput.Create.Binding.map(m => Empty.copy(gnirsSpectroscopy = Some(m))),
        "gnirsSpectroscopy"  -> GnirsSpectroscopyInput.Create.Binding.map(m => Empty.copy(gnirsSpectroscopy = Some(m))),
        "igrins2LongSlit"    -> Igrins2LongSlitInput.Create.Binding.map(m => Empty.copy(igrins2LongSlit = Some(m))),
        "visitor"            -> VisitorInput.CreateBinding.map(m => Empty.copy(visitor = Some(m)))
      )

  final case class Edit(
    exchange:           Option[ExchangeInput.Edit],
    flamingos2Imaging:  Option[Flamingos2ImagingInput.Edit],
    flamingos2LongSlit: Option[Flamingos2LongSlitInput.Edit],
    flamingos2Mos:      Option[Flamingos2MosInput.Edit],
    ghostIfu:           Option[GhostIfuInput.Edit],
    gmosNorthIfu:       Option[GmosIfuInput.Edit.North],
    gmosNorthImaging:   Option[GmosImagingInput.Edit.North],
    gmosNorthLongSlit:  Option[GmosLongSlitInput.Edit.North],
    gmosNorthMos:       Option[GmosMosInput.Edit.North],
    gmosSouthIfu:       Option[GmosIfuInput.Edit.South],
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

    /**
     * The observing mode types a telluric must have to accept this edit, or empty when the
     * edit is not a science `exposureTimeMode` alone.  A telluric of another mode must not
     * be admitted: the edit would replace its observing mode.
     */
    def admittedTelluricModes: List[ObservingModeType] =
      import ObservingModeType.*
      val modes: List[List[ObservingModeType]] = List(
        flamingos2LongSlit.filter(_.isScienceExposureTimeModeOnly).as(List(Flamingos2LongSlit)),
        gnirsSpectroscopy.filter(_.isScienceExposureTimeModeOnly).as(List(GnirsLongSlit, GnirsIfu)),
        igrins2LongSlit.filter(_.isScienceExposureTimeModeOnly).as(List(Igrins2LongSlit))
      ).flatten
      val others: Boolean =
        copy(flamingos2LongSlit = None, gnirsSpectroscopy = None, igrins2LongSlit = None)
          .productIterator.forall(_ == None)
      modes match
        case List(ms) if others => ms
        case _                  => Nil

    def observingModeType: Option[ObservingModeType] =
      exchange.flatMap(_.mode)
        .orElse(flamingos2Imaging.map(_.observingModeType))
        .orElse(flamingos2LongSlit.map(_.observingModeType))
        .orElse(flamingos2Mos.map(_.observingModeType))
        .orElse(ghostIfu.map(_.observingModeType))
        .orElse(gmosNorthIfu.map(_.observingModeType))
        .orElse(gmosNorthImaging.as(ObservingModeType.GmosNorthImaging))
        .orElse(gmosNorthLongSlit.map(_.observingModeType))
        .orElse(gmosNorthMos.map(_.observingModeType))
        .orElse(gmosSouthIfu.map(_.observingModeType))
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
       gmosNorthIfu.traverse(_.toCreate),
       gmosNorthImaging.traverse(_.toCreate),
       gmosNorthLongSlit.traverse(_.toCreate),
       gmosNorthMos.traverse(_.toCreate),
       gmosSouthIfu.traverse(_.toCreate),
       gmosSouthImaging.traverse(_.toCreate),
       gmosSouthLongSlit.traverse(_.toCreate),
       gmosSouthMos.traverse(_.toCreate),
       gnirsImaging.traverse(_.toCreate),
       gnirsSpectroscopy.traverse(_.toCreate),
       igrins2LongSlit.traverse(_.toCreate),
       visitor.traverse(_.toCreate)
      ).parMapN(Create.apply)

  object Edit:

    /** No mode selected.  See `Create.Empty`. */
    val Empty: Edit =
      Edit(
        exchange           = None,
        flamingos2Imaging  = None,
        flamingos2LongSlit = None,
        flamingos2Mos      = None,
        ghostIfu           = None,
        gmosNorthIfu       = None,
        gmosNorthImaging   = None,
        gmosNorthLongSlit  = None,
        gmosNorthMos       = None,
        gmosSouthIfu       = None,
        gmosSouthImaging   = None,
        gmosSouthLongSlit  = None,
        gmosSouthMos       = None,
        gnirsImaging       = None,
        gnirsSpectroscopy  = None,
        igrins2LongSlit    = None,
        visitor            = None
      )

    val Binding: Matcher[Edit] =
      OneOfBinding(
        "exchange"           -> ExchangeInput.EditBinding.map(m => Empty.copy(exchange = Some(m))),
        "flamingos2Imaging"  -> Flamingos2ImagingInput.Edit.Binding.map(m => Empty.copy(flamingos2Imaging = Some(m))),
        "flamingos2LongSlit" -> Flamingos2LongSlitInput.Edit.Binding.map(m => Empty.copy(flamingos2LongSlit = Some(m))),
        "flamingos2Mos"      -> Flamingos2MosInput.Edit.Binding.map(m => Empty.copy(flamingos2Mos = Some(m))),
        "ghostIfu"           -> GhostIfuInput.Edit.Binding.map(m => Empty.copy(ghostIfu = Some(m))),
        "gmosNorthIfu"       -> GmosIfuInput.Edit.North.Binding.map(m => Empty.copy(gmosNorthIfu = Some(m))),
        "gmosNorthImaging"   -> GmosImagingInput.Edit.NorthBinding.map(m => Empty.copy(gmosNorthImaging = Some(m))),
        "gmosNorthLongSlit"  -> GmosLongSlitInput.Edit.North.Binding.map(m => Empty.copy(gmosNorthLongSlit = Some(m))),
        "gmosNorthMos"       -> GmosMosInput.Edit.North.Binding.map(m => Empty.copy(gmosNorthMos = Some(m))),
        "gmosSouthIfu"       -> GmosIfuInput.Edit.South.Binding.map(m => Empty.copy(gmosSouthIfu = Some(m))),
        "gmosSouthImaging"   -> GmosImagingInput.Edit.SouthBinding.map(m => Empty.copy(gmosSouthImaging = Some(m))),
        "gmosSouthLongSlit"  -> GmosLongSlitInput.Edit.South.Binding.map(m => Empty.copy(gmosSouthLongSlit = Some(m))),
        "gmosSouthMos"       -> GmosMosInput.Edit.South.Binding.map(m => Empty.copy(gmosSouthMos = Some(m))),
        "gnirsIfu"           -> GnirsIfuInput.Edit.Binding.map(m => Empty.copy(gnirsSpectroscopy = Some(m))),
        "gnirsImaging"       -> GnirsImagingInput.Edit.Binding.map(m => Empty.copy(gnirsImaging = Some(m))),
        "gnirsLongSlit"      -> GnirsLongSlitInput.Edit.Binding.map(m => Empty.copy(gnirsSpectroscopy = Some(m))),
        "gnirsSpectroscopy"  -> GnirsSpectroscopyInput.Edit.Binding.map(m => Empty.copy(gnirsSpectroscopy = Some(m))),
        "igrins2LongSlit"    -> Igrins2LongSlitInput.Edit.Binding.map(m => Empty.copy(igrins2LongSlit = Some(m))),
        "visitor"            -> VisitorInput.EditBinding.map(m => Empty.copy(visitor = Some(m)))
      )
