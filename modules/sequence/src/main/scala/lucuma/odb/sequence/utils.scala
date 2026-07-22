// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
  val requiredExposures = time.exposureCount.value
  val exposuresPerCycle = cycle.count(isOnSource)
  Either.cond(
    exposuresPerCycle > 0,
    NonNegInt.unsafeFrom((requiredExposures + (exposuresPerCycle - 1)) / exposuresPerCycle),
    "At least one exposure must be on slit (if longslit) or guided (if IFU)."
  )
