// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.Visit
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.ObservationIdBinding
import lucuma.odb.graphql.binding.ObservationReferenceBinding
import lucuma.odb.graphql.binding.VisitIdBinding

/**
 * Selects what to recalculate: a single visit, or every visit of an
 * observation.
 */
sealed trait RecalculateTimeAccountingInput

object RecalculateTimeAccountingInput:

  final case class ForVisit(
    visitId: Visit.Id
  ) extends RecalculateTimeAccountingInput

  final case class ForObservation(
    observationId:  Option[Observation.Id],
    observationRef: Option[ObservationReference]
  ) extends RecalculateTimeAccountingInput

  val Binding: Matcher[RecalculateTimeAccountingInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ObservationIdBinding.Option("observationId", rObservationId),
        ObservationReferenceBinding.Option("observationReference", rObservationRef),
        VisitIdBinding.Option("visitId", rVisitId)
      ) =>
        (rObservationId, rObservationRef, rVisitId).parFlatMapN[RecalculateTimeAccountingInput]:
          case (None, None, Some(vid)) => Result(ForVisit(vid))
          case (oid,  ref,  None)      => Result(ForObservation(oid, ref))
          case _                       => Result.failure("Select either a visit or an observation, not both.")
