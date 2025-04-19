// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.odb.graphql.binding.*

object GmosNorthDynamicInput {

  val Binding: Matcher[GmosNorth] =
    ObjectFieldsBinding.rmap {
      case List(
        TimeSpanInput.Binding("exposure", rExposure),
        GmosCcdModeInput.Binding("readout", rReadout),
        GmosDtaxBinding("dtax", rDtax),
        GmosRoiBinding("roi", rRoi),
        GmosNorthGratingConfigInput.Binding.Option("gratingConfig", rGrating),
        GmosNorthFilterBinding.Option("filter", rFilter),
        GmosNorthFpuInput.Binding.Option("fpu", rFpu)
      ) => (rExposure, rReadout, rDtax, rRoi, rGrating, rFilter, rFpu).parMapN { (exposure, readout, dtax, roi, grating, filter, fpu) =>
        GmosNorth(
          exposure,
          readout,
          dtax,
          roi,
          grating,
          filter,
          fpu
        )
      }
    }

}
