// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.syntax.eq.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.OffsetPurpose
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.data.ProtoStep

/**
 * Computes the number of repeats of `cycle` needed to deliver the
 * `time.frameCount` exposures that were requested, rounding up.  Only steps on
 * the object count toward the requested exposures: sky steps don't contribute
 * to the S/N, so cycles with sky positions require extra repeats.
 */
def calculateCycleCount[D](
  cycle: List[ProtoStep[D]],
  time:  IntegrationTime
): Either[String, NonNegInt] =
  val requiredFrames = time.frameCount.value
  val framesPerCycle = cycle.count(_.telescopeConfig.purpose === OffsetPurpose.Object)
  Either.cond(
    framesPerCycle > 0,
    NonNegInt.unsafeFrom((requiredFrames + (framesPerCycle - 1)) / framesPerCycle),
    "At least one position must be on the object (inside the slit or IFU field)."
  )
