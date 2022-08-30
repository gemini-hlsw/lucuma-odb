// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data
package arb

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

import java.time.Duration
import java.time.temporal.ChronoUnit.MICROS

object ArbTimestamp {

  private val MaxDuration: Duration =
    Duration.between(Timestamp.Min.toInstant, Timestamp.Max.toInstant)

  private val MaxDurationMillis: Long =
    MaxDuration.toMillis

  given arbTimestamp: Arbitrary[Timestamp] =
    Arbitrary {
      Gen.choose[Long](0, ArbTimestamp.MaxDurationMillis).map { ms =>
        Timestamp.unsafeFromInstant(
          Timestamp.Min.toInstant.plusMillis(ms)
        )
      }
    }
}
