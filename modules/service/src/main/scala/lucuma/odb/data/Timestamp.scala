// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order
import cats.syntax.either.*
import cats.syntax.order.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.optics.Format
import org.typelevel.cats.time.instances.instant.*

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset.UTC
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.ISO_DATE_TIME
import java.time.format.DateTimeFormatterBuilder
import java.time.format.DateTimeParseException
import java.time.temporal.ChronoField
import java.time.temporal.ChronoUnit.MICROS
import java.util.Locale

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

  def fromInstant(value: Instant): Option[Timestamp] =
    Option.when(Min <= value && value <= Max && value.truncatedTo(MICROS) === value)(value)

  def unsafeFromInstant(value: Instant): Timestamp =
    fromInstant(value).get

  def fromLocalDateTime(value: LocalDateTime): Option[Timestamp] =
    fromInstant(value.toInstant(UTC))

  def unsafeFromLocalDateTime(value: LocalDateTime): Timestamp =
    fromLocalDateTime(value).get

  def ofEpochMilli(epochMilli: Long): Option[Timestamp] =
    fromInstant(Instant.ofEpochMilli(epochMilli))

  private def formatter(iso: Boolean): DateTimeFormatter = {
    val builder =
      new DateTimeFormatterBuilder()
        .append(DateTimeFormatter.ISO_LOCAL_DATE)
        .appendLiteral(if (iso) then 'T' else ' ')
        .appendPattern("HH:mm:ss")
        .appendFraction(ChronoField.NANO_OF_SECOND, 0, 6, true)

    (if (iso) builder.appendLiteral('Z') else builder).toFormatter(Locale.US)
  }

  val Formatter: DateTimeFormatter =
    formatter(iso = false)

  private val IsoFormatter: DateTimeFormatter =
    formatter(iso = true)

  def parse(s: String): Either[String, Timestamp] =
    Either
      .catchOnly[DateTimeParseException](LocalDateTime.parse(s, Formatter).toInstant(UTC))
      .orElse(Either.catchOnly[DateTimeParseException](LocalDateTime.parse(s, IsoFormatter).toInstant(UTC)))
      .leftMap(_ => s"Could not parse as a Timestamp: $s")
      .flatMap(fromInstant(_).toRight(s"Invalid Timestamp: $s"))

  extension (timestamp: Timestamp) {

    def format: String =
      Formatter.format(toLocalDateTime)

    def toInstant: Instant =
      timestamp

    def toLocalDateTime: LocalDateTime =
      LocalDateTime.ofInstant(timestamp, UTC)

  }

  val FromString: Format[String, Timestamp] =
    Format(parse(_).toOption, Formatter.format)

  val FromInstant: Format[Instant, Timestamp] =
    Format(fromInstant, toInstant)

  val FromLocalDateTime: Format[LocalDateTime, Timestamp] =
    Format(fromLocalDateTime, toLocalDateTime)

  given orderTimestamp: Order[Timestamp] with
    def compare(t0: Timestamp, t1: Timestamp): Int =
      t0.compareTo(t1)

  given decoderTimestamp: Decoder[Timestamp] =
    Decoder.decodeString.emap(parse)

  given encoderTimestamp: Encoder[Timestamp] =
    Encoder.encodeString.contramap[Timestamp](_.format)

}