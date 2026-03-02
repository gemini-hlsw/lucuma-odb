// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep


case class ReplaceSequenceInput[D](
  observationId:  Option[Observation.Id],
  observationRef: Option[ObservationReference],
  sequenceType:   SequenceType,
  sequence:       List[ProtoAtom[ProtoStep[D]]]
)

object ReplaceSequenceInput:

  def binding[D](
    atomBinding: Matcher[ProtoAtom[ProtoStep[D]]]
  ): Matcher[ReplaceSequenceInput[D]] =
    ObjectFieldsBinding.rmap:
      case List(
        ObservationIdBinding.Option("observationId", rObservationId),
        ObservationReferenceBinding.Option("observationReference", rObservationRef),
        SequenceTypeBinding("sequenceType", rSequenceType),
        atomBinding.List("sequence", rSequence)
      ) => (rObservationId, rObservationRef, rSequenceType, rSequence).parMapN: (oid, ref, seqType, seq) =>
        ReplaceSequenceInput(oid, ref, seqType, seq)

  val ReplaceFlamingos2Binding: Matcher[ReplaceSequenceInput[Flamingos2DynamicConfig]] =
    binding(AtomInput.Flamingos2Binding)

  val ReplaceGmosNorthBinding: Matcher[ReplaceSequenceInput[GmosNorth]] =
    binding(AtomInput.GmosNorthBinding)

  val ReplaceGmosSouthBinding: Matcher[ReplaceSequenceInput[GmosSouth]] =
    binding(AtomInput.GmosSouthBinding)