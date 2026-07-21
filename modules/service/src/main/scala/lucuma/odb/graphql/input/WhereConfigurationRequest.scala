// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model.ConfigurationRequest
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.binding.WhereOrder
import lucuma.odb.graphql.binding.WhereOptionString

object WhereConfigurationRequest {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderConfigurationRequestId = WhereOrder.binding(path / "id", ConfigurationRequestIdBinding)
    val WhereStatusBinding = WhereOrder.binding(path / "status", enumeratedBinding[ConfigurationRequestStatus])
    val WhereProgramBinding = WhereProgram.binding(path / "program")
    val WhereJustificationBinding = WhereOptionString.binding(path / "justification")
    val WhereFeedbackBinding = WhereOptionString.binding(path / "feedback")
    val WhereCreatedAtBinding = WhereOrder.binding[Timestamp](path / "createdAt", TimestampBinding)
    val WhereUpdatedAtBinding = WhereOrder.binding[Timestamp](path / "updatedAt", TimestampBinding)

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
      ) =>
        (rAND, rOR, rNOT, rId, rStatus, rProgram, rJustification, rFeedback, rCreatedAt, rUpdatedAt).parMapN {
          (AND, OR, NOT, id, status, program, justification, feedback, createdAt, updatedAt) =>
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
            ).flatten)
        }
    }
  }

}
