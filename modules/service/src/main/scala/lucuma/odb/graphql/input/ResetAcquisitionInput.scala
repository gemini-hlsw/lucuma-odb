// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.ObservationIdBinding
import lucuma.odb.graphql.binding.ObservationReferenceBinding

final case class ResetAcquisitionInput(
  observationId:  Option[Observation.Id],
  observationRef: Option[ObservationReference],
  regenerate:     Boolean
)

object ResetAcquisitionInput:

  val Binding: Matcher[ResetAcquisitionInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ObservationIdBinding.Option("observationId", rObservationId),
        ObservationReferenceBinding.Option("observationReference", rObservationRef),
        BooleanBinding.Option("regenerate", rRegenerate)
      ) =>
        (rObservationId, rObservationRef, rRegenerate.map(_.getOrElse(false))).parMapN(ResetAcquisitionInput.apply)
