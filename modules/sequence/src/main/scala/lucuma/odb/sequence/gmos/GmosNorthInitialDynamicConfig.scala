// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos

import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosDtax
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.TimeSpan

trait GmosNorthInitialDynamicConfig {

  /**
   * Starting point, default dynamic configuration for GMOS North.  This will
   * serve as the initial state in state computations that produce sequence
   * steps.
   */
  val initialConfig: GmosNorth =
    GmosNorth(
      exposure = TimeSpan.Min,
      readout  = GmosCcdMode(
        GmosXBinning.One,
        GmosYBinning.One,
        GmosAmpCount.Twelve,
        GmosAmpGain.Low,
        GmosAmpReadMode.Fast
      ),
      dtax          = GmosDtax.Zero,
      roi           = GmosRoi.FullFrame,
      gratingConfig = None,
      filter        = None,
      fpu           = None
    )

}
