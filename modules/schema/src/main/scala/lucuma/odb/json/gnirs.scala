// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import coulomb.Quantity
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStep
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.util.TimeSpan

import time.decoder.given
import wavelength.decoder.given

trait GnirsCodec:

  given Decoder[GnirsStaticConfig] =
    Decoder.instance: c =>
      for
        wd <- c.downField("wellDepth").as[GnirsWellDepth]
      yield GnirsStaticConfig(wd)

  given Encoder[GnirsStaticConfig] =
    Encoder.instance: a =>
      Json.obj(
        "wellDepth" -> a.wellDepth.asJson,
      )

  given Decoder[GnirsAcquisitionMirrorMode.Out] =
    Decoder.instance: c =>
      for
        prism <- c.downField("prism").as[GnirsPrism]
        grating <- c.downField("grating").as[GnirsGrating]
        wavelength <- c.downField("wavelength").as[GnirsGratingWavelength]
      yield GnirsAcquisitionMirrorMode.Out(prism, grating, wavelength)

  given (using Encoder[Wavelength]): Encoder[GnirsAcquisitionMirrorMode.Out] =
    Encoder.instance: a =>
      Json.obj(
        "prism" -> a.prism.asJson,
        "grating" -> a.grating.asJson,
        "wavelength" -> a.wavelength.asJson
      )

  given Decoder[GnirsAcquisitionMirrorMode] =
    Decoder.instance: c =>
      c.as[Option[GnirsAcquisitionMirrorMode.Out]].map(_.getOrElse(GnirsAcquisitionMirrorMode.In))

  given (using Encoder[Wavelength]): Encoder[GnirsAcquisitionMirrorMode] =
    Encoder.instance: a =>
      GnirsAcquisitionMirrorMode.out.getOption(a).fold(Json.Null)(_.asJson)

  given Decoder[GnirsFocus.Custom] =
    Decoder.instance: c =>
      c.as[GnirsFocusMotorStepsValue].map: motorSteps =>
        GnirsFocus.Custom(motorSteps.withUnit[GnirsFocusMotorStep])

  given Encoder[GnirsFocus.Custom] =
    Encoder.instance: a =>
      a.value.value.asJson

  given Decoder[GnirsFocus] =
    Decoder.instance: c =>
      c.as[Option[GnirsFocus.Custom]].map(_.getOrElse(GnirsFocus.Best))

  given Encoder[GnirsFocus] =
    Encoder.instance: a =>
      GnirsFocus.custom.getOption(a).fold(Json.Null)(_.asJson)

  given Decoder[GnirsDynamicConfig] =
    Decoder.instance: c =>
      for
        exposure          <- c.downField("exposure").as[TimeSpan]
        coadds            <- c.downField("coadds").as[PosInt]
        filter            <- c.downField("filter").as[GnirsFilter]
        decker            <- c.downField("decker").as[GnirsDecker]
        fpu               <- c.downField("fpuSlit").as[GnirsFpuSlit].map(GnirsFpu.Spectroscopy.Slit(_)) orElse
                             c.downField("fpuOther").as[GnirsFpuOther].map(GnirsFpu.Other(_))
        acqMirror         <- c.downField("acquisitionMirrorOut").as[GnirsAcquisitionMirrorMode]
        camera            <- c.downField("camera").as[GnirsCamera]
        focus             <- c.downField("focusMotorSteps").as[GnirsFocus]
        readMode          <- c.downField("readMode").as[GnirsReadMode]
      yield GnirsDynamicConfig(
        exposure,
        coadds,
        filter,
        decker,
        fpu,
        acqMirror,
        camera,
        focus,
        readMode
      )

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[GnirsDynamicConfig] =
    Encoder.instance: a =>
      Json.obj(
        "exposure"             -> a.exposure.asJson,
        "coadds"               -> a.coadds.asJson,
        "centralWavelength"    -> a.centralWavelength.asJson,
        "filter"               -> a.filter.asJson,
        "decker"               -> a.decker.asJson,
        "fpuSlit"              -> GnirsFpu.slit.getOption(a.fpu).fold(Json.Null)(_.asJson),
        "fpuOther"             -> GnirsFpu.other.getOption(a.fpu).fold(Json.Null)(_.asJson),
        "acquisitionMirrorOut" -> a.acquisitionMirror.asJson,
        "camera"               -> a.camera.asJson,
        "focusMotorSteps"      -> a.focus.asJson,
        "readMode"             -> a.readMode.asJson
      )

object gnirs extends GnirsCodec
