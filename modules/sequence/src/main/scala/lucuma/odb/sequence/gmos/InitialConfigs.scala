// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos

import cats.syntax.option.*
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosDtax
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthStageMode
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthStageMode
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.MosPreImaging
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.util.TimeSpan

object InitialConfigs:

  /**
   * Starting point, default dynamic configuration for GMOS North.  This will
   * serve as the initial state in state computations that produce sequence
   * steps.
   */
  val GmosNorthDynamic: DynamicConfig.GmosNorth =
    DynamicConfig.GmosNorth(
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
      gratingConfig = none,
      filter        = none,
      fpu           = none
    )

  /**
   * Starting point, default dynamic configuration for GMOS South.  This will
   * serve as the initial state in state computations that produce sequence
   * steps.
   */
  val GmosSouthDynamic: DynamicConfig.GmosSouth =
    DynamicConfig.GmosSouth(
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
      gratingConfig = none,
      filter        = none,
      fpu           = none
    )

  val GmosNorthStatic: StaticConfig.GmosNorth =
    StaticConfig.GmosNorth(
      GmosNorthStageMode.FollowXy,
      GmosNorthDetector.Hamamatsu,
      MosPreImaging.IsNotMosPreImaging,
      none
    )

  val GmosSouthStatic: StaticConfig.GmosSouth =
    StaticConfig.GmosSouth(
      GmosSouthStageMode.FollowXyz,
      GmosSouthDetector.Hamamatsu,
      MosPreImaging.IsNotMosPreImaging,
      none
    )