// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order
import cats.parse.Parser
import cats.parse.Parser.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.Encoder
import io.circe.syntax.*
import lucuma.core.enums.Half
import lucuma.core.parser.MiscParsers.intN
import lucuma.core.parser.MiscParsers.posInt
import lucuma.core.model.Semester
import lucuma.core.optics.Format

import java.time.DateTimeException
import java.time.Year
import scala.util.control.Exception

case class ProposalReference(semester: Semester, index: PosInt) {

  def format: String =
    f"G-${semester.format}-$index%04d"

  def formatShort: String =
    f"${semester.formatShort}$index%04d"

}

object ProposalReference {

  given Order[ProposalReference] =
    Order.by { a => (
      a.semester,
      a.index
    )}

  private object parser {
    val semesterYear: Parser[Year] =
      posInt.mapFilter { yr =>
        Exception.catching(classOf[DateTimeException]).opt {
          Year.of(yr.value)
        }
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
      char('0').rep.void *> posInt

    val fullFormat: Parser[ProposalReference] =
      ((char('G').void *> semester.surroundedBy(char('-'))) ~ index).map { (semester, index) =>
        ProposalReference(semester, index)
      }

    val shortFormat: Parser[ProposalReference] =
      (shortSemester ~ index).map { (semester, index) =>
        ProposalReference(semester, index)
      }
  }

  val FromString: Format[String, ProposalReference] =
    Format(s => parser.fullFormat.parseAll(s).toOption, _.format)
    
  val FromShortString: Format[String, ProposalReference] =
    Format(s => parser.shortFormat.parseAll(s).toOption, _.formatShort)

  given Decoder[ProposalReference] =
    Decoder.decodeString.emap { s =>
      FromString
        .getOption(s)
        .orElse(FromShortString.getOption(s))
        .toRight(s"Could not parse '$s' as a proposal reference.")
    }

  given Encoder[ProposalReference] =
    Encoder.instance(_.format.asJson)

}

