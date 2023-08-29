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
  instrument:   Instrument,
  visitId:      Visit.Id,
  sequenceType: SequenceType,
  stepCount:    NonNegShort
)

object RecordAtomInput {

  def bindingFor(instrument: Instrument): Matcher[RecordAtomInput] =
    ObjectFieldsBinding.rmap {
      case List(
        VisitIdBinding("visitId", rVisitId),
        SequenceTypeBinding("sequenceType", rSequenceType),
        NonNegShortBinding("stepCount", rStepCount)
      ) => (rVisitId, rSequenceType, rStepCount).parMapN { (visitId, sequenceType, stepCount) =>
        RecordAtomInput(instrument, visitId, sequenceType, stepCount)
      }
    }

}


