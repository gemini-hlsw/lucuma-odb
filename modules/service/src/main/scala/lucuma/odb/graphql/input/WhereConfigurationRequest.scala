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
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.binding.WhereOptionString
import lucuma.odb.graphql.binding.WhereOrder

object WhereConfigurationRequest {

  def binding(path: Path): Matcher[Predicate] = {
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
    def observingModeTypeBinding(binding: Matcher[ObservingModeType]): Matcher[Predicate] =
      val modePath = path / "configuration" / "observingMode" / "mode"
      ObjectFieldsBinding.rmap:
        case List(
          BooleanBinding.Option("IS_NULL", rIsNull),
          binding.Option("EQ", rEQ),
          binding.Option("NEQ", rNEQ),
          binding.List.Option("IN", rIN),
          binding.List.Option("NIN", rNIN)
        ) =>
          (rIsNull, rEQ, rNEQ, rIN, rNIN).parMapN: (isNull, EQ, NEQ, IN, NIN) =>
            and(List(
              isNull.map(IsNull(modePath, _)),
              EQ.map(a => Eql(modePath, Const(a))),
              NEQ.map(a => NEql(modePath, Const(a))),
              IN.map(as => In(modePath, as)),
              NIN.map(as => Not(In(modePath, as)))
            ).flatten)

    val ObservingModeTypeBinding = observingModeTypeBinding(enumeratedBinding[ObservingModeType])

    // The cone's candidate lookup is an effect the elaborator cannot run, so this yields a
    // placeholder that `ConeFilter.resolve` swaps for `id IN (…)` before execution. Parsing
    // it as an ordinary binding means variables and fragments are already resolved here.
    val TargetCoordinatesBinding: Matcher[Predicate] =
      ObjectFieldsBinding.rmap:
        case List(
          CoordinatesInput.Create.Binding("center", rCenter),
          AngleInput.Binding("distance", rDistance)
        ) => (rCenter, rDistance).parMapN(ConePredicate(path / "id", _, _))

    lazy val WhereObservationBinding = binding(path) // lazy self-reference
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

}
