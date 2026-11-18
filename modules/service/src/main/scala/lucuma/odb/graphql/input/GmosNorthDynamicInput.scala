// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.odb.graphql.binding.*

object GmosNorthDynamicInput {

  /** @see [[GmosCustomMaskInput.OptionalWidthBinding]] */
  val OptionalWidthBinding: Matcher[(GmosNorth, Boolean)] =
    ObjectFieldsBinding.rmap {
      case List(
        TimeSpanInput.Binding("exposure", rExposure),
        GmosCcdModeInput.Binding("readout", rReadout),
        GmosDtaxBinding("dtax", rDtax),
        GmosRoiBinding("roi", rRoi),
        GmosNorthGratingConfigInput.Binding.Option("gratingConfig", rGrating),
        GmosNorthFilterBinding.Option("filter", rFilter),
        GmosNorthFpuInput.OptionalWidthBinding.Option("fpu", rFpu)
      ) => (rExposure, rReadout, rDtax, rRoi, rGrating, rFilter, rFpu).parMapN { (exposure, readout, dtax, roi, grating, filter, fpu) =>
        (GmosNorth(
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

  val Binding: Matcher[GmosNorth] =
    OptionalWidthBinding.rmap {
      case (config, false) => Result(config)
      case (_, true)       => Result.failure(GmosCustomMaskInput.SlitWidthRequiredMessage)
    }

}
