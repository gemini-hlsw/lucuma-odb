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

import scala.annotation.tailrec

/**
 * GMOS long slit science atoms
 *
 * @tparam D dynamic config type
 * @tparam G grating type
 * @tparam F filter type
 * @tparam U FPU type
 */
sealed trait Science[D, G, F, U] extends ScienceAtomSequenceState[D, G, F, U] {

  override def stream(
    mode:          Config[G, F, U],
    exposureTime:  SciExposureTime
  ): Stream[Pure, ScienceAtom[D]] = {

    @tailrec def gcd(a: BigInt, b: BigInt): BigInt = if (b === 0) a else gcd(b, a%b)
    def lcm(as: BigInt*): BigInt = as.reduce { (a, b) => a/gcd(a,b)*b }

    // How many potentially unique configs will be generated.
    val uniqueConfigCount = lcm(
      2, // to account for alternation between (science, flat), (flat, science)
      math.max(mode.wavelengthDithers.size, 1),
      math.max(mode.spatialOffsets.size, 1)
    )

    val Δλs  = mode.wavelengthDithers match {
      case Nil => Stream(WavelengthDither.Zero).repeat
      case ws  => Stream.emits(ws).repeat
    }
    val qs   = mode.spatialOffsets match {
      case Nil => Stream(Offset.Q.Zero).repeat
      case os  => Stream.emits(os).repeat
    }

    val seq = unfold(mode, exposureTime, Δλs, qs)

    // If the number of unique configs is reasonably small (as it almost always
    // should be), pre-generate them and then just repeat.  Otherwise, we'll
    // just (re)generate steps as we go.
    if (uniqueConfigCount > 100) seq
    else Stream.emits(seq.take(uniqueConfigCount.toLong).toList).repeat

  }

}

object Science {

  object GmosNorth extends GmosNorthInitialDynamicConfig
                      with Science[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] {

    override def optics: DynamicOptics[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
      DynamicOptics.North

  }

  object GmosSouth extends GmosSouthInitialDynamicConfig
                      with Science[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] {

    override def optics: DynamicOptics[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] =
      DynamicOptics.South

  }

}
