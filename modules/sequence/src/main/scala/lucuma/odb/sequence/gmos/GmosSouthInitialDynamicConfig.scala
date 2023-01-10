// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos

import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosDtax
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.model.NonNegDuration
import lucuma.core.model.sequence.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.GmosCcdMode

trait GmosSouthInitialDynamicConfig {

  /**
   * Starting point, default dynamic configuration for GMOS South.  This will
   * serve as the initial state in state computations that produce sequence
   * steps.
   */
  val initialConfig: GmosSouth =
    GmosSouth(
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
