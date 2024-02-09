// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order
import cats.parse.Parser
import cats.parse.Parser.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.cats.given
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.Encoder
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Semester
import lucuma.core.optics.Format
import monocle.Prism


sealed trait ProgramReference extends Product with Serializable {
  def programClass: ProgramClass
  def format: String
}

object ProgramReference {

  type Description = String Refined MatchesRegex["""^[A-Z0-9]+"""]
  object Description extends RefinedTypeOps[Description, String]

  case class Calibration(semester: Semester, instrument: Instrument, index: PosInt) extends ProgramReference {
    override def programClass: ProgramClass =
      ProgramClass.Calibration

    override def format: String =
      f"G-${semester.format}-${programClass.abbreviation}-${instrument.tag}-$index%02d"
  }

  object Calibration {

    given Order[Calibration] =
      Order.by { a => (a.semester, a.instrument, a.index) }

    val fromString: Format[String, Calibration] =
      Format(s => parse.calibration.parseAll(s).toOption, _.format)

  }

  case class Engineering(semester: Semester, instrument: Instrument, index: PosInt) extends ProgramReference {
    override def programClass: ProgramClass =
      ProgramClass.Engineering

    override def format: String =
      f"G-${semester.format}-${programClass.abbreviation}-${instrument.tag}-$index%02d"
  }

  object Engineering {

    given Order[Engineering] =
      Order.by { a => (a.semester, a.instrument, a.index) }

    val fromString: Format[String, Engineering] =
      Format(s => parse.engineering.parseAll(s).toOption, _.format)

  }

  case class Example(instrument: Instrument) extends ProgramReference {
    override def programClass: ProgramClass =
      ProgramClass.Example

    override def format: String =
      s"G-${programClass.abbreviation}-${instrument.tag}"
  }

  object Example {

    given Order[Example] =
      Order.by(_.instrument)

    val fromString: Prism[String, Example] =
      Prism[String, Example](s => parse.example.parseAll(s).toOption)(_.format)

  }

  case class Library(instrument: Instrument, description: Description) extends ProgramReference {
    override def programClass: ProgramClass =
      ProgramClass.Library

    override def format: String =
      s"G-${programClass.abbreviation}-${instrument.tag}-${description.value}"
  }

  object Library {

    given Order[Library] =
      Order.by { a => (a.instrument, a.description.value) }

    val fromString: Prism[String, Library] =
      Prism[String, Library](s => parse.library.parseAll(s).toOption)(_.format)

  }

  case class Science(proposal: ProposalReference, scienceType: ScienceType) extends ProgramReference {
    override def programClass: ProgramClass =
      ProgramClass.Science

    override def format: String =
      s"${proposal.format}-${scienceType.letter}"
  }

  object Science {

    given Order[Science] =
      Order.by { a => (a.proposal, a.scienceType) }

    val fromString: Format[String, Science] =
      Format(s => parse.program.parseAll(s).toOption, _.format)
  }

  object parse {

    import ReferenceParsers.*

    val description: Parser[Description] =
      charsWhile { c => (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') }
        .mapFilter(s => Description.from(s).toOption)

    def semesterInstruentIndex[A](abbr: String)(f: (Semester, Instrument, PosInt) => A): Parser[A] =
      (semester.surroundedBy(dash).between(G, string(abbr)) ~ instrument.surroundedBy(dash) ~ index)
        .map { case ((semester, instrument), index) => f(semester, instrument, index) }

    val calibration: Parser[Calibration] =
      semesterInstruentIndex(ProgramClass.Calibration.abbreviation)(Calibration.apply)

    val engineering: Parser[Engineering] =
      semesterInstruentIndex(ProgramClass.Engineering.abbreviation)(Engineering.apply)

    val example: Parser[Example] =
      (string(s"G-${ProgramClass.Example.abbreviation}-") *> instrument).map { instrument =>
        Example(instrument)
      }

    val library: Parser[Library] =
      (instrument.between(string(s"G-${ProgramClass.Library.abbreviation}-"), dash) ~ description)
        .map { case (instrument, description) => Library(instrument, description) }

    val program: Parser[Science] =
      ((ProposalReference.parse.proposal <* dash) ~ scienceType).map { case (proposal, scienceType) =>
        Science(proposal, scienceType)
      }

    val programReference: Parser[ProgramReference] =
      oneOf(
        parse.program.backtrack     ::
        parse.calibration.backtrack ::
        parse.engineering.backtrack ::
        parse.example.backtrack     ::
        parse.library               ::
        Nil
      )
  }

  val fromString: Format[String, ProgramReference] =
    Format(s => parse.programReference.parseAll(s).toOption, _.format)

  given Decoder[ProgramReference] =
    Decoder.decodeString.emap { s =>
      fromString
        .getOption(s)
        .toRight(s"Could not parse '$s' as a ProgramReference.")
    }

  given Encoder[ProgramReference] =
    Encoder.instance(_.format.asJson)

  given Order[ProgramReference] =
    Order.from {
      case (a @ Calibration(_, _, _), b @ Calibration(_, _, _)) => Order.compare(a, b)
      case (a @ Engineering(_, _, _), b @ Engineering(_, _, _)) => Order.compare(a, b)
      case (a @ Example(_),           b @ Example(_))           => Order.compare(a, b)
      case (a @ Library(_, _),        b @ Library(_, _))        => Order.compare(a, b)
      case (a @ Science(_, _),        b @ Science(_, _))        => Order.compare(a, b)
      case (a, b) => Order.compare(a.programClass, b.programClass)
    }
}

