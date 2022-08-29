// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order
import cats.syntax.order._

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset.UTC
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit.MICROS
import lucuma.core.optics.Format
import org.typelevel.cats.time.instances.instant._

/**
 * Timestamp is an Instant truncated and limited to fit in a database
 * `timestamp` column.  Using a `Timestamp`, we're guaranteed to safely
 * round-trip values through the database.
 */

opaque type Timestamp = Instant

object Timestamp {

  val Min: Timestamp =
    ZonedDateTime.of( -4712, 1, 1, 0, 0, 0, 0, UTC).toInstant

  val Max: Timestamp =
    ZonedDateTime.of(294275, 12, 31, 23, 59, 59, 999999000, UTC).toInstant

  def fromInstant(value: Instant): Option[Timestamp] = {
    val valueʹ = value.truncatedTo(MICROS)
    Option.when(Min <= valueʹ && valueʹ <= Max)(valueʹ)
  }

  def fromLocalDateTime(value: LocalDateTime): Option[Timestamp] =
    fromInstant(value.toInstant(UTC))

  def ofEpochMilli(epochMilli: Long): Option[Timestamp] =
    fromInstant(Instant.ofEpochMilli(epochMilli))

  extension (timestamp: Timestamp) {

    def toInstant: Instant =
      timestamp

    def toLocalDateTime: LocalDateTime =
      LocalDateTime.ofInstant(timestamp, UTC)

  }

  given orderTimestamp: Order[Timestamp] with
    def compare(t0: Timestamp, t1: Timestamp): Int =
      t0.compareTo(t1)

  val FromInstant: Format[Instant, Timestamp] =
    Format(fromInstant, toInstant)

  val FromLocalDateTime: Format[LocalDateTime, Timestamp] =
    Format(fromLocalDateTime, toLocalDateTime)

}