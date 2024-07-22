// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.syntax.eq.*
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.math.Offset
import lucuma.core.math.WavelengthDither
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.odb.sequence.data.SciExposureTime

/**
 * GMOS long slit spectro photometric calibration atoms
 *
 * @tparam D dynamic config type
 * @tparam G grating type
 * @tparam F filter type
 * @tparam U FPU type
 */
sealed trait SpectroPhotometric[D, G, F, U] extends ScienceAtomSequenceState[D, G, F, U] {

  def compute(
    mode:         Config[G, F, U],
    exposureTime: SciExposureTime
  ): Stream[Pure, ScienceAtom[D]] =

    val limit   = mode.coverage.toPicometers.value.value / 10.0
    val dithers = if (mode.wavelengthDithers.exists(_.toPicometers.value.abs > limit)) mode.wavelengthDithers else List(WavelengthDither.Zero)
    val Δλs     = Stream.emits(dithers).repeat
    val qs      = Stream(Offset.Q.Zero).repeat

    sequence(mode, exposureTime, Δλs, qs).take(dithers.length)

}

object SpectroPhotometric {

  object GmosNorth extends GmosNorthInitialDynamicConfig
                      with SpectroPhotometric[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] {

    override def optics: DynamicOptics[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
      DynamicOptics.North

  }

  object GmosSouth extends GmosSouthInitialDynamicConfig
                      with SpectroPhotometric[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] {

    override def optics: DynamicOptics[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] =
      DynamicOptics.South

  }

}
