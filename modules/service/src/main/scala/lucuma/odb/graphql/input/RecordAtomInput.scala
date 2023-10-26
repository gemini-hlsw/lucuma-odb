// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Visit
import lucuma.odb.graphql.binding.*

case class RecordAtomInput(
  visitId:      Visit.Id,
  instrument:   Instrument,
  sequenceType: SequenceType,
  stepCount:    NonNegShort
)

object RecordAtomInput {

  val Binding: Matcher[RecordAtomInput] =
    ObjectFieldsBinding.rmap {
      case List(
        VisitIdBinding("visitId", rVisitId),
        InstrumentBinding("instrument", rInstrument),
        SequenceTypeBinding("sequenceType", rSequenceType),
        NonNegShortBinding("stepCount", rStepCount)
      ) => (rVisitId, rInstrument, rSequenceType, rStepCount).parMapN { (visitId, instrument, sequenceType, stepCount) =>
        RecordAtomInput(visitId, instrument, sequenceType, stepCount)
      }
    }

}


