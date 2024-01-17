// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.parse.*
import cats.syntax.all._
import lucuma.core.enums.parser.EnumParsers.*
import lucuma.core.model.Semester
import lucuma.core.parser.MiscParsers.intN
import lucuma.core.parser.TimeParsers.*

import java.time.Year


private [binding] object semesterBinding {
  val MinSemester: Semester =
    Semester.unsafeFromString("2000A")

  // Support for semesters specified as 24A etc.  There's a Y2100 problem here
  // that we can deal with in 2099.

  private val year2: Parser[Year] =
    intN(2).mapFilter(yr => catchDTE(Year.of)(2000 + yr)).withContext("year2")

  private val shortSemester: Parser[Semester] =
    (year2, half).mapN(Semester.apply).withContext("short semester")

  def fromShortString(s: String): Option[Semester] =
    shortSemester.parseAll(s).toOption
}


val SemesterBinding: Matcher[Semester] = {
  import semesterBinding.*

  StringBinding.emap { s =>
    Semester
      .fromString(s)
      .orElse(fromShortString(s))
      .filter(_ >= MinSemester)
      .toRight(s"`$s` cannot be parsed as a valid Semester.")
    }
}