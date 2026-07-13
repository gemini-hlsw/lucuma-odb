// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.syntax.eq.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.data.ProtoStep

def isOnSlit(slitLength: Angle, o: Offset): Boolean =
  (o.p.toAngle.toMicroarcseconds === 0L) &&
  (Angle.signedDecimalArcseconds.get(o.q.toAngle).abs <= (Angle.signedDecimalArcseconds.get(slitLength) / 2))

def isGuided(guiding: StepGuideState): Boolean = 
  guiding match
    case StepGuideState.Enabled  => true
    case StepGuideState.Disabled => false

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
