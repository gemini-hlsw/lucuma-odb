// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order
import cats.parse.Parser
import cats.parse.Parser.*
import cats.syntax.option.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.Encoder
import io.circe.syntax.*
import lucuma.core.enums.Half
import lucuma.core.model.Semester
import lucuma.core.optics.Format
import lucuma.core.parser.MiscParsers.intN
import lucuma.core.parser.MiscParsers.posInt

import java.time.DateTimeException
import java.time.Year
import scala.util.control.Exception

case class ProgramReference(semester: Semester, index: PosInt) {

  def format: String =
    f"G-${semester.format}-$index%04d"

  def formatShort: String =
    f"${semester.formatShort}$index%04d"

}

object ProgramReference {

  given Order[ProgramReference] =
    Order.by { a => (
      a.semester,
      a.index
    )}

  object parse {

    val semesterYear: Parser[Year] =
      intN(4).mapFilter {
        case yr if yr >= 2000 =>
          Exception.catching(classOf[DateTimeException]).opt {
            Year.of(yr)
          }
        case _ => none
      }

    val shortSemesterYear: Parser[Year] =
      intN(2).mapFilter { yr =>
        Exception.catching(classOf[DateTimeException]).opt {
          Year.of(yr + 2000)
        }
      }

    val half: Parser[Half] =
      char('A').as(Half.A) | char('B').as(Half.B)

    val semester: Parser[Semester] =
      (semesterYear ~ half).map(Semester.apply)

    val shortSemester: Parser[Semester] =
      (shortSemesterYear ~ half).map(Semester.apply)

    val index: Parser[PosInt] =
      char('0').rep0.with1 *> posInt

    val fullFormat: Parser[ProgramReference] =
      ((char('G').void *> semester.surroundedBy(char('-'))) ~ index).map { (semester, index) =>
        ProgramReference(semester, index)
      }

    val shortFormat: Parser[ProgramReference] =
      (shortSemester ~ index).map { (semester, index) =>
        ProgramReference(semester, index)
      }
  }

  val fromString: Format[String, ProgramReference] =
    Format(s => parse.fullFormat.parseAll(s).toOption, _.format)

  val fromShortString: Format[String, ProgramReference] =
    Format(s => parse.shortFormat.parseAll(s).toOption, _.formatShort)

  given Decoder[ProgramReference] =
    Decoder.decodeString.emap { s =>
      fromString
        .getOption(s)
        .orElse(fromShortString.getOption(s))
        .toRight(s"Could not parse '$s' as a proposal reference.")
    }

  given Encoder[ProgramReference] =
    Encoder.instance(_.format.asJson)

}

