// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.PortDisposition
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.binding.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

final case class GnirsImagingInput(
  exposureTimeMode: ExposureTimeMode,
  filter:           GnirsFilter,
  camera:           GnirsCamera,
  readMode:         GnirsReadMode,
  wellDepth:        GnirsWellDepth,
  coadds:           PosInt,
  port:             PortDisposition
) extends InstrumentModesInput

object GnirsImagingInput:

  def binding: Matcher[GnirsImagingInput] =
    ObjectFieldsBinding.rmap:
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode),
            GnirsFilterBinding("filter", filter),
            GnirsCameraBinding("camera", camera),
            GnirsReadModeBinding("readMode", readMode),
            GnirsWellDepthBinding("wellDepth", wellDepth),
            PosIntBinding.Option("coadds", coadds),
            PortDispositionBinding("port", portDisposition)
          ) =>
        // coadds is optional for compatibility with clients that don't send it; default to 1.
        (exposureTimeMode,
         filter,
         camera,
         readMode,
         wellDepth,
         coadds.map(_.getOrElse(PosInt.MinValue)),
         portDisposition
        ).parMapN(apply)
