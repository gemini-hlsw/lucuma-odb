// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.Target
import lucuma.odb.data.CloneSequenceMode
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

final case class CloneObservationInput(
  observationId:  Option[Observation.Id],
  observationRef: Option[ObservationReference],
  SET:            Option[ObservationPropertiesInput.Edit],
  sequence:       CloneSequenceMode
) {

  def asterism: Nullable[NonEmptyList[Target.Id]] =
    SET.fold(Nullable.Absent)(_.asterism)

}

object CloneObservationInput {

  val CloneSequenceModeBinding: Matcher[CloneSequenceMode] =
    enumeratedBinding[CloneSequenceMode]

  val Binding: Matcher[CloneObservationInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ObservationIdBinding.Option("observationId", rObservationId),
        ObservationReferenceBinding.Option("observationReference", rObservationRef),
        ObservationPropertiesInput.Edit.Binding.Option("SET", rSET),
        CloneSequenceModeBinding.Option("sequence", rSequence)
      ) =>
        (rObservationId, rObservationRef, rSET, rSequence).mapN: (oid, ref, set, seq) =>
          CloneObservationInput(oid, ref, set, seq.getOrElse(CloneSequenceMode.None))
    }

}
