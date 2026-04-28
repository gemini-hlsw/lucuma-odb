// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.input.ExposureTimeModeInput
import lucuma.core.enums.PortDisposition
import lucuma.itc.binding.*
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPixelScale
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsPrism
import lucuma.core.math.Wavelength
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.GnirsFpuSlit

final case class GnirsSpectroscopyInput(
  exposureTimeMode:  ExposureTimeMode.TimeAndCountMode,
  centralWavelength: Wavelength,
  grating:           GnirsGrating,
  pixelScale:        GnirsPixelScale,
  prism:             GnirsPrism,
  readMode:          GnirsReadMode,
  slitWidth:         GnirsFpuSlit,
  wellDepth:         GnirsWellDepth,
  port:              PortDisposition
) extends InstrumentModesInput

// def gnirsParameters(r: ITCRequest): GnirsParameters = {
//   val grating     = r.parameter("Disperser") match {
//     case "imaging" => None
//     case _ => Some(r.enumParameter(classOf[GNIRSParams.Disperser])) }
//   val filter      = r.parameter("Filter") match {
//     case "spectroscopy" => None
//     case _ => Some(r.enumParameter(classOf[GNIRSParams.Filter])) }
//   val pixelScale = r.enumParameter(classOf[GNIRSParams.PixelScale])
//   val xDisp       = r.enumParameter(classOf[GNIRSParams.CrossDispersed])
//   val readMode    = r.enumParameter(classOf[GNIRSParams.ReadMode])
//   val centralWl   = r.centralWavelengthInMicrons()
//   val fpMask      = r.enumParameter(classOf[GNIRSParams.SlitWidth])
//   val wellDepth   = r.enumParameter(classOf[GNIRSParams.WellDepth])
//   val camera      = None                            //    are selected automatically and not controlled by user
//   val altair      = altairParameters(r)
//   GnirsParameters(pixelScale, filter, grating, readMode, xDisp, centralWl, fpMask, camera, wellDepth, altair)
// }

object GnirsSpectroscopyInput:

  def binding: Matcher[GnirsSpectroscopyInput] = ???
  // ObjectFieldsBinding.rmap:
  //   case List(
  //         ExposureTimeModeInput.TimeAndCount.Binding("exposureTimeMode", exposureTimeMode),
  //         PortDispositionBinding("port", portDisposition)
  //       ) =>
  //     (exposureTimeMode, portDisposition).parMapN(apply)
