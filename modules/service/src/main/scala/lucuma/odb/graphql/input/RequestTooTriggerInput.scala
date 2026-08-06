// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.odb.graphql.binding.*

case class RequestTooTriggerInput(
  observationId:  Option[Observation.Id],
  observationRef: Option[ObservationReference]
)

object RequestTooTriggerInput:

  val Binding: Matcher[RequestTooTriggerInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ObservationIdBinding.Option("observationId", rObsId),
        ObservationReferenceBinding.Option("observationReference", rObsRef)
      ) =>
        (rObsId, rObsRef).parMapN(apply)
