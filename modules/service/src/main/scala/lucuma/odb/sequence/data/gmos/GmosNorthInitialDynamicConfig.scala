// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data.gmos

import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosDtax
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.model.NonNegDuration
import lucuma.core.model.sequence.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.GmosCcdMode

trait GmosNorthInitialDynamicConfig {

  /**
   * Starting point, default dynamic configuration for GMOS North.  This will
   * serve as the initial state in state computations that produce sequence
   * steps.
   */
  val initialConfig: GmosNorth =
    GmosNorth(
      exposure = NonNegDuration.zero.value,
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
