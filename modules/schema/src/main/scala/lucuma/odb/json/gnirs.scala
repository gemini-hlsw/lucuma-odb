// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.util.TimeSpan
import io.circe.refined.*
import coulomb.syntax.*

import time.decoder.given
import wavelength.transport.given
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsDecker
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsGrating
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsCamera
import lucuma.core.model.sequence.gnirs.GnirsFocus
import coulomb.Quantity
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStep
import lucuma.core.enums.GnirsReadMode
import eu.timepit.refined.types.numeric.PosInt

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

  given Encoder[GnirsAcquisitionMirrorMode.Out] =
    Encoder.instance: a =>
      Json.obj(
        "prism" -> a.prism.asJson,
        "grating" -> a.grating.asJson,
        "wavelength" -> a.wavelength.asJson
      )

  given Decoder[GnirsFocus.Custom] =
    Decoder.instance: c =>
      c.as[GnirsFocusMotorStepsValue].map: motorSteps =>
        GnirsFocus.Custom(motorSteps.withUnit[GnirsFocusMotorStep])

  given Encoder[GnirsFocus.Custom] =
    Encoder.instance: a =>
      a.value.value.asJson

  given Decoder[GnirsDynamicConfig] =
    Decoder.instance: c =>
      for
        exposure  <- c.downField("exposure").as[TimeSpan]
        coadds    <- c.downField("coadds").as[PosInt]
        filter    <- c.downField("filter").as[GnirsFilter]
        decker    <- c.downField("decker").as[GnirsDecker]
        fpu       <- c.downField("fpuSlit").as[GnirsFpuSlit].map(_.asLeft[GnirsFpuOther]) orElse
                     c.downField("fpuOther").as[GnirsFpuOther].map(_.asRight[GnirsFpuSlit])
        acqMirror <- c.downField("acquisitionMirrorOut").as[Option[GnirsAcquisitionMirrorMode.Out]].map(_.getOrElse(GnirsAcquisitionMirrorMode.In))
        camera    <- c.downField("camera").as[GnirsCamera]
        focus     <- c.downField("focusCustom").as[Option[GnirsFocus.Custom]].map(_.getOrElse(GnirsFocus.Best))
        readMode  <- c.downField("readMode").as[GnirsReadMode]
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

  given (using Encoder[TimeSpan]): Encoder[GnirsDynamicConfig] =
    Encoder.instance: a =>
      Json.obj(
        "exposure"             -> a.exposure.asJson,
        "coadds"               -> a.coadds.asJson,
        "filter"               -> a.filter.asJson,
        "decker"               -> a.decker.asJson,
        "fpuSlit"              -> a.fpu.left.toOption.fold(Json.Null)(_.asJson),
        "fpuOther"             -> a.fpu.toOption.fold(Json.Null)(_.asJson),
        "acquisitionMirrorOut" -> GnirsAcquisitionMirrorMode.out.getOption(a.acquisitionMirror).fold(Json.Null)(_.asJson),
        "camera"               -> a.camera.asJson,
        "focus"                -> GnirsFocus.custom.getOption(a.focus).fold(Json.Null)(_.asJson),
        "readMode"             -> a.readMode.asJson
      )

object gnirs extends GnirsCodec
