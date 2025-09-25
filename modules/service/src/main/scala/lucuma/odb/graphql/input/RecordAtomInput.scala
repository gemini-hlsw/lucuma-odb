// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.util.IdempotencyKey
import lucuma.odb.graphql.binding.*

case class RecordAtomInput(
  visitId:        Visit.Id,
  instrument:     Instrument,
  sequenceType:   SequenceType,
  generatedId:    Option[Atom.Id],
  idempotencyKey: Option[IdempotencyKey]
)

object RecordAtomInput:

  val Binding: Matcher[RecordAtomInput] =
    ObjectFieldsBinding.rmap:
      case List(
        VisitIdBinding("visitId", rVisitId),
        InstrumentBinding("instrument", rInstrument),
        SequenceTypeBinding("sequenceType", rSequenceType),
        AtomIdBinding.Option("generatedId", rGenerated),
        IdempotencyKeyBinding.Option("idempotencyKey", rIdm)
      ) => (rVisitId, rInstrument, rSequenceType, rGenerated, rIdm).parMapN:
        (visitId, instrument, sequenceType, generated, idm) =>
          RecordAtomInput(visitId, instrument, sequenceType, generated, idm)