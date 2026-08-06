// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.Timestamp
import lucuma.odb.data.TooTrigger
import lucuma.odb.data.TooTriggerStatus
import lucuma.odb.graphql.binding.*

object WhereTooTrigger:

  def binding(path: Path): Matcher[Predicate] =
    val WhereOrderTooTriggerId     = WhereOrder.binding[TooTrigger.Id](path / "id", gidBinding[TooTrigger.Id]("too trigger"))
    val WhereObservationIdBinding  = WhereOrder.binding[Observation.Id](path / "observation" / "id", ObservationIdBinding)
    val WhereProgramIdBinding      = WhereOrder.binding[Program.Id](path / "programId", ProgramIdBinding)
    val WhereStatusBinding         = WhereOrder.binding[TooTriggerStatus](path / "status", enumeratedBinding[TooTriggerStatus])
    val WhereRequestedAtBinding    = WhereOrder.binding[Timestamp](path / "requestedAt", TimestampBinding)
    val WhereRequestedByBinding    = WhereUser.binding(path / "requestedBy")
    val WhereUpdatedAtBinding      = WhereOrder.binding[Timestamp](path / "updatedAt", TimestampBinding)

    lazy val WhereTooTriggerBinding = binding(path)

    ObjectFieldsBinding.rmap:
      case List(
        WhereTooTriggerBinding.List.Option("AND", rAND),
        WhereTooTriggerBinding.List.Option("OR", rOR),
        WhereTooTriggerBinding.Option("NOT", rNOT),
        WhereOrderTooTriggerId.Option("id", rId),
        WhereObservationIdBinding.Option("observationId", rObs),
        WhereProgramIdBinding.Option("programId", rProgram),
        WhereStatusBinding.Option("status", rStatus),
        WhereRequestedAtBinding.Option("requestedAt", rRequestedAt),
        WhereRequestedByBinding.Option("requestedBy", rRequestedBy),
        WhereUpdatedAtBinding.Option("updatedAt", rUpdatedAt)
      ) =>
        (rAND, rOR, rNOT, rId, rObs, rProgram, rStatus, rRequestedAt, rRequestedBy, rUpdatedAt).parMapN:
          (AND, OR, NOT, id, obs, program, status, requestedAt, requestedBy, updatedAt) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              id,
              obs,
              program,
              status,
              requestedAt,
              requestedBy,
              updatedAt
            ).flatten)
