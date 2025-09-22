// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input.customSed

import cats.Order.given
import cats.data.NonEmptyMap
import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.core.math.Wavelength
import lucuma.core.model.Attachment

import scala.collection.immutable.SortedMap

/**
 * Implementation of `CustomSed.Resolver` that parses a `dat` file, consisting of lines of the form
 * "`wavelength density`".
 *
 * This trait just provides the parsing functionality, and does not specify how to obtain the `dat`
 * file.
 */
trait CustomSedDatResolver[F[_]: Concurrent] extends CustomSed.Resolver[F]:
  protected def datLines(id: Attachment.Id): F[fs2.Stream[F, String]]

  // delimiters are whitespaces, commas or semicolons
  private val Delimiters = "(\\s|,|;)+"
  private val EmptyLine  = "\\s*"
  private val Comment    = "\\s*#.*"

  private def parseLine(line: String): Either[String, (Wavelength, BigDecimal)] =
    line.split(Delimiters).take(2) match
      case Array(wavelength, density) =>
        val w: Either[String, Wavelength] =
          wavelength.toDoubleOption
            .flatMap:
              Wavelength.decimalNanometers.getOption(_)
            .toRight(s"Invalid wavelength in custom SED: [$wavelength].")
        val d: Either[String, BigDecimal] =
          density.toDoubleOption
            .map(BigDecimal(_))
            .toRight(s"Invalid density in custom SED: [$density].")
        (w, d).parTupled
      case _                          =>
        Left(s"Invalid line in custom SED: [$line].")

  def resolve(id: Attachment.Id): F[NonEmptyMap[Wavelength, BigDecimal]] =
    for
      lines <- datLines(id)
      pairs <-
        lines
          .filterNot(_.matches(EmptyLine))
          .filterNot(_.matches(Comment))
          .map(parseLine)
          .evalMap(_.leftMap(new RuntimeException(_)).liftTo[F])
          .compile
          .toList
      map    = SortedMap.from(pairs)
      nem   <- NonEmptyMap
                 .fromMap(map)
                 .toRight(new RuntimeException(s"Custom SED file for id [$id] is empty."))
                 .liftTo[F]
    yield nem
