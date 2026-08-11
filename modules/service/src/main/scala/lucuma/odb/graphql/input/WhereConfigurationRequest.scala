// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.ConfigurationRequest
import lucuma.core.util.Timestamp
import lucuma.odb.data.Cone
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.binding.WhereOptionEq
import lucuma.odb.graphql.binding.WhereOptionString
import lucuma.odb.graphql.binding.WhereOrder

object WhereConfigurationRequest:

  /** The WHERE input shared by the `configurationRequests` query and the
   *  `updateConfigurationRequests` mutation.
   *
   *  @param `allowCone` says whether `targetCoordinates` may be used. A cone elaborates to a
   *  `ConePredicate` that `ConeFilter` resolves by walking the compiled query, so it only
   *  works where the predicate ends up in the query tree. Thus it is only usable on queries,
   *  not mutations.
   */
  def binding(path: Path, allowCone: Boolean): Matcher[Predicate] = {
    val WhereOrderConfigurationRequestId = WhereOrder.binding(path / "id", ConfigurationRequestIdBinding)
    val WhereStatusBinding = WhereOrder.binding(path / "status", enumeratedBinding[ConfigurationRequestStatus])
    val WhereProgramBinding = WhereProgram.binding(path / "program")
    val WhereJustificationBinding = WhereOptionString.binding(path / "justification")
    val WhereFeedbackBinding = WhereOptionString.binding(path / "feedback")
    val WhereCreatedAtBinding = WhereOrder.binding[Timestamp](path / "createdAt", TimestampBinding)
    val WhereUpdatedAtBinding = WhereOrder.binding[Timestamp](path / "updatedAt", TimestampBinding)

    // A configuration request exposes its observing mode type nested under its
    // `configuration.observingMode`, so the predicate path is one segment deeper
    // than the equivalent field on `WhereObservation`.
    val ObservingModeTypeBinding = WhereOptionEq.unwrappedBinding(path / "configuration" / "observingMode" / "mode", enumeratedBinding[ObservingModeType])

    // The cone's candidate lookup is an effect the elaborator cannot run, so this yields a
    // placeholder that `ConeFilter.resolve` swaps for `id IN (…)` before execution. Parsing
    // it as an ordinary binding means variables are already substituted here.
    val TargetCoordinatesBinding: Matcher[Predicate] =
      if !allowCone then
        _ => Left("`targetCoordinates` is only supported when querying configuration requests.")
      else
        ObjectFieldsBinding.rmap:
          case List(
            CoordinatesInput.Create.Binding("center", rCenter),
            AngleInput.Binding("distance", rDistance)
          ) => (rCenter, rDistance).parMapN((c, d) => ConeFilter.ConePredicate(path / "id", Cone(c, d)))

    lazy val WhereObservationBinding = binding(path, allowCone) // lazy self-reference
    ObjectFieldsBinding.rmap {
      case List(
        WhereObservationBinding.List.Option("AND", rAND),
        WhereObservationBinding.List.Option("OR", rOR),
        WhereObservationBinding.Option("NOT", rNOT),
        WhereOrderConfigurationRequestId.Option("id", rId),
        WhereProgramBinding.Option("program", rProgram),
        WhereStatusBinding.Option("status", rStatus),
        WhereJustificationBinding.Option("justification", rJustification),
        WhereFeedbackBinding.Option("feedback", rFeedback),
        WhereCreatedAtBinding.Option("createdAt", rCreatedAt),
        WhereUpdatedAtBinding.Option("updatedAt", rUpdatedAt),
        ObservingModeTypeBinding.Option("observingModeType", rObservingModeType),
        TargetCoordinatesBinding.Option("targetCoordinates", rTargetCoordinates),
      ) =>
        (rAND, rOR, rNOT, rId, rStatus, rProgram, rJustification, rFeedback, rCreatedAt, rUpdatedAt, rObservingModeType, rTargetCoordinates).parMapN {
          (AND, OR, NOT, id, status, program, justification, feedback, createdAt, updatedAt, observingModeType, targetCoordinates) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              id,
              status,
              program,
              justification,
              feedback,
              createdAt,
              updatedAt,
              observingModeType,
              targetCoordinates,
            ).flatten)
        }
    }
  }
