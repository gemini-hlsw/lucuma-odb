// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.odb.graphql.binding.*

final case class SetGuideTargetNameInput(
  observationId: Option[Observation.Id],
  observationRef: Option[ObservationReference],
  targetName:    Option[NonEmptyString]
)

object SetGuideTargetNameInput {
  val Binding: Matcher[SetGuideTargetNameInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ObservationIdBinding.Option("observationId", rObsId),
        ObservationReferenceBinding.Option("observationReference", rObsRef),
        NonEmptyStringBinding.Option("targetName", rName)
      ) => (rObsId, rObsRef, rName).parMapN(apply)
    }
}
