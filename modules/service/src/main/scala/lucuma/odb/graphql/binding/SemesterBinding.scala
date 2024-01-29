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

// TODO: reuse the parser in ProgramReference

private [binding] object semesterBinding {
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
      .toRight(s"'$s' cannot be parsed as a valid Semester.")
    }
}