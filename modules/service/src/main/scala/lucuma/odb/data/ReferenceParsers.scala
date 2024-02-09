// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.parse.Parser
import cats.parse.Parser.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Half
import lucuma.core.enums.Instrument
import lucuma.core.enums.parser.EnumParsers.enumBy
import lucuma.core.model.Semester
import lucuma.core.parser.MiscParsers.intN
import lucuma.core.parser.MiscParsers.posInt

import java.time.DateTimeException
import java.time.Year
import scala.util.control.Exception


trait ReferenceParsers {

  val G: Parser[Unit] =
    char('G').void

  val dash: Parser[Unit] =
    char('-').void

  val semesterYear: Parser[Year] =
    intN(4).mapFilter {
      case yr if yr >= 2000 =>
        Exception.catching(classOf[DateTimeException]).opt {
          Year.of(yr)
        }
      case _ => none
    }

  val half: Parser[Half] =
    char('A').as(Half.A) | char('B').as(Half.B)

  val instrument: Parser[Instrument] =
    enumBy[Instrument](_.tag)

  val semester: Parser[Semester] =
    (semesterYear ~ half).map(Semester.apply)

  val index: Parser[PosInt] =
    char('0').rep0.with1 *> posInt

  val scienceType: Parser[ScienceType] =
    charIn(ScienceType.values.map(_.letter)).mapFilter(ScienceType.fromLetter)

}

object ReferenceParsers extends ReferenceParsers