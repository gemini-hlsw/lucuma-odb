// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.data

import cats.Order
import eu.timepit.refined.types.numeric.PosInt
import org.typelevel.cats.time.*

import java.time.Instant

/**
 * SmartGcal definition file version.  This is found in the first line of every
 * file and consists of an increasing positive integer and a time stamp.
 * @param number
 * @param time
 */
case class FileVersion(
  number: PosInt,
  time:   Instant
)

object FileVersion {

  given Order[FileVersion] =
    Order.by { a => (
      a.number.value,
      a.time
    )}

}
