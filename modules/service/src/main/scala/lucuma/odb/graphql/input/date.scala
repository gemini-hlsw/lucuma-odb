// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.Ior
import cats.syntax.order.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.util.DateInterval
import lucuma.odb.graphql.binding.Matcher
import org.typelevel.cats.time.*

import java.time.LocalDate
import java.time.Month
import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE

object date:

  // Our code for computing the LST only works until 2100 apparently.

  val DefaultStart: LocalDate       = LocalDate.of(1901, Month.JANUARY,   1)
  val DefaultEnd: LocalDate         = LocalDate.of(2099, Month.DECEMBER, 31)
  val DefaultInterval: DateInterval = DateInterval.between(DefaultStart, DefaultEnd)

  def validate(name: String, date: LocalDate): Result[LocalDate] =
    if ((date >= DefaultStart) && (date <= DefaultEnd)) date.success
    else Matcher.validationFailure(s"'$name' date (${date.format(ISO_LOCAL_DATE)}) must be between 1900 and 2100 UTC (exclusive)")

  def validateInput(name: String, rDate: Result[LocalDate]): Result[LocalDate] =
    rDate.flatMap(validate(name, _))

  def validateOptionalInput(name: String, rDate: Result[Option[LocalDate]]): Result[Option[LocalDate]] =
    rDate.flatMap(_.traverse(validate(name, _)))

  def validateInputInterval(
    startName: String,
    endName:   String,
    rStart:    Result[LocalDate],
    rEnd:      Result[LocalDate]
  ): Result[DateInterval] =
    (validateInput(startName, rStart),
     validateInput(endName, rEnd)
    ).parTupled.flatMap: (start, end) =>
      Result.fromOption(
        Option.when(start < end)(DateInterval.between(start, end)),
        Matcher.validationProblem(s"'$startName' must come before '$endName'")
      )

  def validateOptionalInputInterval(
    startName: String,
    endName:   String,
    rStart:    Result[Option[LocalDate]],
    rEnd:      Result[Option[LocalDate]]
  ): Result[Option[Ior[LocalDate, LocalDate]]] =
    (validateOptionalInput(startName, rStart),
     validateOptionalInput(endName, rEnd)
    ).parTupled.flatMap:
      case (Some(s), Some(e)) if s > e =>
        Matcher.validationFailure(s"'$startName' must come before '$endName'")
      case (s, e)                      =>
        Result(Ior.fromOptions(s, e))