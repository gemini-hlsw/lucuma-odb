// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order
import cats.parse.Parser
import cats.parse.Parser.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.Encoder
import io.circe.syntax.*
import lucuma.core.model.ProgramReference
import lucuma.core.model.parser.ReferenceParsers
import lucuma.core.optics.Format

case class ObservationReference(
  programReference: ProgramReference,
  observationIndex: PosInt
) {

  /** Formatted observation reference String. */
  def label: String =
    f"${programReference.label}-$observationIndex%04d"

}

object ObservationReference {

  object parse {
    import ProgramReference.parse.programReference
    import ReferenceParsers.*

    val observation: Parser[ObservationReference] =
      ((programReference <* dash) ~ index).map { (ref, index) =>
        ObservationReference(ref, index)
      }
  }

  given Order[ObservationReference] =
    Order.by { a => (a.programReference, a.observationIndex.value) }

  given Ordering[ObservationReference] =
    Order[ObservationReference].toOrdering

  val fromString: Format[String, ObservationReference] =
    Format(s => parse.observation.parseAll(s).toOption, _.label)

  given Decoder[ObservationReference] =
    Decoder.decodeString.emap { s =>
      fromString
        .getOption(s)
        .toRight(s"Could not parse '$s' as an ObservationReference.")
    }

  given Encoder[ObservationReference] =
    Encoder.instance(_.label.asJson)

}
