// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.model.NonNegDuration

import java.time.Duration

opaque type SciExposureTime = NonNegDuration

object SciExposureTime {
  
  extension (expTime: SciExposureTime) {
    def duration: NonNegDuration = expTime

    def *(that: NonNegInt): SciExposureTime =
      NonNegDuration.unsafeFrom(expTime.value.multipliedBy(that.value.toLong))
  }
  
  def apply(duration: NonNegDuration): SciExposureTime = duration

  def fromDuration(duration: Duration): Option[SciExposureTime] =
    NonNegDuration.unapply(duration)
      
}

opaque type AcqExposureTime = NonNegDuration

object AcqExposureTime {

  extension (expTime: AcqExposureTime) {
    def duration: NonNegDuration = expTime

    def *(that: NonNegInt): SciExposureTime =
      NonNegDuration.unsafeFrom(expTime.value.multipliedBy(that.value.toLong))
  }

  def apply(duration: NonNegDuration): AcqExposureTime = duration

  def fromDuration(duration: Duration): Option[AcqExposureTime] =
    NonNegDuration.unapply(duration)
      
}




