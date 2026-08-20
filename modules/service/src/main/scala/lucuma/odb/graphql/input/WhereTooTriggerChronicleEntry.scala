// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.util.Timestamp
import lucuma.odb.data.DatabaseOperation
import lucuma.odb.data.TooTrigger
import lucuma.odb.graphql.binding.*

object WhereTooTriggerChronicleEntry:

  def binding(path: Path): Matcher[Predicate] =

    val WhereOrderChronicleId    = WhereOrder.binding[Long](path / "id", LongBinding)
    val WhereUserBinding         = WhereUser.binding(path / "user")
    val WhereEqDatabaseOperation = WhereEq.binding[DatabaseOperation](path / "operation", DatabaseOperationBinding)
    val WhereOrderTimestamp      = WhereOrder.binding[Timestamp](path / "timestamp", TimestampBinding)
    val WhereOrderTooTriggerId   = WhereOrder.binding[TooTrigger.Id](path / "tooTrigger" / "id", gidBinding[TooTrigger.Id]("too trigger"))

    val WhereModObservationId    = WhereBoolean.binding(path / "modObservationId", BooleanBinding)
    val WhereModProgramId        = WhereBoolean.binding(path / "modProgramId", BooleanBinding)
    val WhereModStatus           = WhereBoolean.binding(path / "modStatus", BooleanBinding)
    val WhereModResolutionReason = WhereBoolean.binding(path / "modResolutionReason", BooleanBinding)
    val WhereModTooActivation    = WhereBoolean.binding(path / "modTooActivation", BooleanBinding)
    val WhereModSupersedes       = WhereBoolean.binding(path / "modSupersedes", BooleanBinding)

    lazy val WhereTooTriggerChronicleEntryBinding = binding(path)

    ObjectFieldsBinding.rmap:
      case List(
        WhereTooTriggerChronicleEntryBinding.List.Option("AND", rAND),
        WhereTooTriggerChronicleEntryBinding.List.Option("OR", rOR),
        WhereTooTriggerChronicleEntryBinding.Option("NOT", rNOT),

        WhereOrderChronicleId.Option("id", rId),
        WhereUserBinding.Option("user", rUser),
        WhereEqDatabaseOperation.Option("operation", rOp),
        WhereOrderTimestamp.Option("timestamp", rTimestamp),
        WhereOrderTooTriggerId.Option("tooTrigger", rTooTriggerId),

        WhereModObservationId.Option("modObservationId", rModObservationId),
        WhereModProgramId.Option("modProgramId", rModProgramId),
        WhereModStatus.Option("modStatus", rModStatus),
        WhereModResolutionReason.Option("modResolutionReason", rModResolutionReason),
        WhereModTooActivation.Option("modTooActivation", rModTooActivation),
        WhereModSupersedes.Option("modSupersedes", rModSupersedes)
      ) =>
        (rAND, rOR, rNOT, rId, rUser, rOp, rTimestamp, rTooTriggerId, rModObservationId, rModProgramId, rModStatus, rModResolutionReason, rModTooActivation, rModSupersedes).parMapN:
          (AND, OR, NOT, id, user, op, timestamp, tooTriggerId, modObservationId, modProgramId, modStatus, modResolutionReason, modTooActivation, modSupersedes) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              id,
              user,
              op,
              timestamp,
              tooTriggerId,
              modObservationId,
              modProgramId,
              modStatus,
              modResolutionReason,
              modTooActivation,
              modSupersedes
            ).flatten)
