// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Parser0
import cats.parse.Rfc5234.alpha
import cats.parse.Rfc5234.wsp
import cats.syntax.apply.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan

import java.time.Instant
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale.US
import scala.util.control.Exception.allCatch

// TODO: lucuma-core has a MiscParsers that some of these could go to I suppose
trait CommonParsers {

  val maybeWhiteSpace: Parser0[Unit] =
    wsp.rep0.void

  val comma: Parser[Unit] =
    Parser.char(',').withContext("comma")

  val columnSep: Parser[Unit] =
    comma.surroundedBy(maybeWhiteSpace)

  val dash: Parser[Unit] =
    Parser.char('-').withContext("dash")

  val int: Parser[Int] =
    Numbers.signedIntString.mapFilter { s => allCatch.opt(s.toInt) }

  val posInt: Parser[PosInt] =
    (Numbers.nonZeroDigit ~ Numbers.digits0)
      .string
      .mapFilter { s => allCatch.opt(s.toInt).flatMap(PosInt.unapply) }

  val posBigDecimal: Parser0[PosBigDecimal] =
    (Numbers.digits0 ~ (Parser.char('.') ~ Numbers.digits).?)
      .string
      .mapFilter { s =>
        allCatch.opt(BigDecimal(s)).flatMap(PosBigDecimal.unapply)
      }

  val posSeconds: Parser0[TimeSpan] =
    posBigDecimal.mapFilter(pbd => TimeSpan.fromSeconds(pbd.value))

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
      BoundedInterval.unsafeClosed(Wavelength.Min, Wavelength.fromIntPicometers(Int.MaxValue - 1).get)  // TODO: add a max wavelength interval to core
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
