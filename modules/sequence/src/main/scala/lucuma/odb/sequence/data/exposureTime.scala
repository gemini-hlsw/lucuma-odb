// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.util.TimeSpan

import java.time.Duration

opaque type SciExposureTime = TimeSpan

object SciExposureTime {
  
  extension (expTime: SciExposureTime) {
    def timeSpan: TimeSpan = expTime

    def *(that: NonNegInt): SciExposureTime =
      TimeSpan.unsafeFromMicroseconds(
        expTime.toMicroseconds * that.value
      )

  }
  
  def apply(timeSpan: TimeSpan): SciExposureTime = timeSpan

  def fromDuration(duration: Duration): Option[SciExposureTime] =
    TimeSpan.FromDuration.getOption(duration)

}

opaque type AcqExposureTime = TimeSpan

object AcqExposureTime {

  extension (expTime: AcqExposureTime) {
    def timeSpan: TimeSpan = expTime

    def *(that: NonNegInt): AcqExposureTime =
      TimeSpan.unsafeFromMicroseconds(
        expTime.toMicroseconds * that.value
      )
  }

  def apply(timeSpan: TimeSpan): AcqExposureTime = timeSpan

  def fromDuration(duration: Duration): Option[AcqExposureTime] =
    TimeSpan.FromDuration.getOption(duration)
      
}




