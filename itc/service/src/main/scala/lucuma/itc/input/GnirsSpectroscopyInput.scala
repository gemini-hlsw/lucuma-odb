// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.binding.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

final case class GnirsSpectroscopyInput(
  exposureTimeMode:  ExposureTimeMode.TimeAndCountMode,
  centralWavelength: Wavelength,
  filter:            GnirsFilter,
  slitWidth:         GnirsFpuSlit,
  prism:             GnirsPrism,
  grating:           GnirsGrating,
  camera:            GnirsCamera,
  readMode:          GnirsReadMode,
  wellDepth:         GnirsWellDepth,
  port:              PortDisposition
) extends InstrumentModesInput

object GnirsSpectroscopyInput:

  def binding: Matcher[GnirsSpectroscopyInput] =
    ObjectFieldsBinding.rmap:
      case List(
            ExposureTimeModeInput.TimeAndCount.Binding("timeAndCount", exposureTimeMode),
            WavelengthInput.Binding("centralWavelength", centralWavelength),
            GnirsFilterBinding("filter", filter),
            GnirsFpuSlitBinding("slitWidth", slitWidth),
            GnirsPrismBinding("prism", prism),
            GnirsGratingBinding("grating", grating),
            GnirsCameraBinding("camera", camera),
            GnirsReadModeBinding("readMode", readMode),
            GnirsWellDepthBinding("wellDepth", wellDepth),
            PortDispositionBinding("port", portDisposition)
          ) =>
        (exposureTimeMode,
         centralWavelength,
         filter,
         slitWidth,
         prism,
         grating,
         camera,
         readMode,
         wellDepth,
         portDisposition
        ).parMapN(apply)
