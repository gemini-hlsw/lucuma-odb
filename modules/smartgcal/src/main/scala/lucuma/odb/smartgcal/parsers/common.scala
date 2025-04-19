// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.parse.Parser
import cats.parse.Rfc5234.alpha
import cats.syntax.apply.*
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.parser.MiscParsers.comma
import lucuma.core.parser.MiscParsers.dash
import lucuma.core.parser.MiscParsers.int
import lucuma.core.parser.MiscParsers.maybeWhiteSpace

import java.time.Instant
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale.US
import scala.util.control.Exception.allCatch

trait CommonParsers {

  val columnSep: Parser[Unit] =
    comma.surroundedBy(maybeWhiteSpace)

  val instant: Parser[Instant] =
    val dtf = DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss zz", US)
    alpha.rep.string.mapFilter { s =>
      allCatch.opt(ZonedDateTime.parse(s, dtf).toInstant)
    }.withContext("date and time")

  val openUpperIntRange: Parser[BoundedInterval[Int]] =
    ((int <* maybeWhiteSpace <* dash <* maybeWhiteSpace) ~ int)
      .mapFilter { case (min, max) =>
         BoundedInterval.openUpper(min, max)
      }.withContext("int range")

  def wavelengthRange(fromInt: Int => Option[Wavelength]): Parser[BoundedInterval[Wavelength]] =
    (Parser.string("Any") <* maybeWhiteSpace).as(
      // From postgres documentation:
      //
      //   The built-in range types int4range, int8range, and daterange all use
      //   a canonical form that includes the lower bound and excludes the upper
      //   bound; that is, [).
      //
      // This implies that the [1, Int.MaxValue] cannot be used, since it would
      // be converted to the "canonical" form [1, Int.MaxValue+1) which won't
      // fit in an int4.
      //BoundedInterval.unsafeClosed(Wavelength.Min, Wavelength.fromIntPicometers(Int.MaxValue).get)
      BoundedInterval.unsafeOpenUpper(Wavelength.Min, Wavelength.fromIntPicometers(Int.MaxValue).get)
    ) |
      openUpperIntRange.mapFilter { interval =>
        (fromInt(interval.lower), fromInt(interval.upper)).mapN { (low, up) =>
          BoundedInterval.unsafeOpenUpper(low, up)
        }
      }.withContext("wavelength range")

  val wavelengthRangeNm: Parser[BoundedInterval[Wavelength]] =
    wavelengthRange(Wavelength.fromIntNanometers)
}

object common extends CommonParsers
