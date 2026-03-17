// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.syntax.eq.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.data.ProtoStep

def isOnSlit(slitLength: Angle, o: Offset): Boolean =
  (o.p.toAngle.toMicroarcseconds === 0L) &&
  (Angle.signedDecimalArcseconds.get(o.q.toAngle).abs <= (Angle.signedDecimalArcseconds.get(slitLength) / 2))

def cycleCount[D](
  slitLength: Angle,
  cycle:      List[ProtoStep[D]],
  time:       IntegrationTime
): Either[String, NonNegInt] =
  val requiredExposures = time.exposureCount.value
  val exposuresPerCycle = cycle.count(s => isOnSlit(slitLength, s.telescopeConfig.offset))
  Either.cond(
    exposuresPerCycle > 0,
    NonNegInt.unsafeFrom((requiredExposures + (exposuresPerCycle - 1)) / exposuresPerCycle),
    "At least one exposure must be taken on slit."
  )
