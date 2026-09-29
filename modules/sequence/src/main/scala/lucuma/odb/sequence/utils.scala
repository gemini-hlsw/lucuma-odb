// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.data.ProtoStep

def calculateCycleCount[D](
  isOnSource: ProtoStep[D] => Boolean,
  cycle:      List[ProtoStep[D]],
  time:       IntegrationTime
): Either[String, NonNegInt] =
  val requiredFrames = time.frameCount.value
  val framesPerCycle = cycle.count(isOnSource)
  Either.cond(
    framesPerCycle > 0,
    NonNegInt.unsafeFrom((requiredFrames + (framesPerCycle - 1)) / framesPerCycle),
    "At least one exposure must be on slit (if longslit) or guided (if IFU)."
  )
