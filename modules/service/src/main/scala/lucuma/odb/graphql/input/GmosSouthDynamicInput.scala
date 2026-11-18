// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.odb.graphql.binding.*

object GmosSouthDynamicInput {

  /** @see [[GmosCustomMaskInput.OptionalWidthBinding]] */
  val OptionalWidthBinding: Matcher[(GmosSouth, Boolean)] =
    ObjectFieldsBinding.rmap {
      case List(
        TimeSpanInput.Binding("exposure", rExposure),
        GmosCcdModeInput.Binding("readout", rReadout),
        GmosDtaxBinding("dtax", rDtax),
        GmosRoiBinding("roi", rRoi),
        GmosSouthGratingConfigInput.Binding.Option("gratingConfig", rGrating),
        GmosSouthFilterBinding.Option("filter", rFilter),
        GmosSouthFpuInput.OptionalWidthBinding.Option("fpu", rFpu)
      ) => (rExposure, rReadout, rDtax, rRoi, rGrating, rFilter, rFpu).parMapN { (exposure, readout, dtax, roi, grating, filter, fpu) =>
        (GmosSouth(
          exposure,
          readout,
          dtax,
          roi,
          grating,
          filter,
          fpu.map(_._1)
        ), fpu.exists(_._2))
      }
    }

  val Binding: Matcher[GmosSouth] =
    OptionalWidthBinding.rmap {
      case (config, false) => Result(config)
      case (_, true)       => Result.failure(GmosCustomMaskInput.SlitWidthRequiredMessage)
    }

}
