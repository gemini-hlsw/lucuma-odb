// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsReadMode
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.gnirs.longslit.Config as GnirsLongSlit

object InitialConfigs:

  /**
   * Starting point dynamic configuration for GNIRS. Edited by sequence
   * generators to produce per-step configurations.
   */
  val GnirsDynamic: GnirsDynamicConfig =
    GnirsDynamicConfig(
      exposure          = TimeSpan.Min,
      coadds            = PosInt.MinValue,
      filter            = GnirsFilter.Y,
      decker            = GnirsDecker.Acquisition,
      fpu               = GnirsFpu.Spectroscopy.Slit(lucuma.core.enums.GnirsFpuSlit.LongSlit_0_10),
      acquisitionMirror = GnirsAcquisitionMirrorMode.In,
      camera            = GnirsCamera.ShortBlue,
      focus             = GnirsFocus.Best,
      readMode          = GnirsReadMode.VeryBright
    )

  def staticFrom(config: GnirsLongSlit): GnirsStaticConfig =
    GnirsStaticConfig(config.wellDepth)
