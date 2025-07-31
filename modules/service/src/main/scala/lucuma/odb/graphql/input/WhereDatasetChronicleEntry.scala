// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.Timestamp
import lucuma.odb.data.DatabaseOperation
import lucuma.odb.graphql.binding.*

object WhereDatasetChronicleEntry:

  def binding(path: Path): Matcher[Predicate] =

    val WhereOrderChronicleId    = WhereOrder.binding[Long](path / "id", LongBinding)
    val WhereUserBinding         = WhereUser.binding(path / "user")
    val WhereEqDatabaseOperation = WhereEq.binding[DatabaseOperation](path / "operation", DatabaseOperationBinding)
    val WhereOrderTimestamp      = WhereOrder.binding[Timestamp](path / "timestamp", TimestampBinding)
    val WhereOrderDatasetId      = WhereOrder.binding[Dataset.Id](path / "dataset" / "id", DatasetIdBinding)

    val WhereModDatasetId        = WhereBoolean.binding(path / "modDatasetId", BooleanBinding)
    val WhereModStepId           = WhereBoolean.binding(path / "modStepId", BooleanBinding)
    val WhereModObservationId    = WhereBoolean.binding(path / "modObservationId", BooleanBinding)
    val WhereModVisitId          = WhereBoolean.binding(path / "modVisitId", BooleanBinding)

    val WhereModReference        = WhereBoolean.binding(path / "modReference", BooleanBinding)
    val WhereModFilename         = WhereBoolean.binding(path / "modFilename", BooleanBinding)
    val WhereModQaState          = WhereBoolean.binding(path / "modQaState", BooleanBinding)
    val WhereModInterval         = WhereBoolean.binding(path / "modInterval", BooleanBinding)
    val WhereModComment          = WhereBoolean.binding(path / "modComment", BooleanBinding)

    lazy val WhereDatasetChronicleEntryBinding = binding(path)

    ObjectFieldsBinding.rmap:
      case List(
        WhereDatasetChronicleEntryBinding.List.Option("AND", rAND),
        WhereDatasetChronicleEntryBinding.List.Option("OR", rOR),
        WhereDatasetChronicleEntryBinding.Option("NOT", rNOT),

        WhereOrderChronicleId.Option("id", rId),
        WhereUserBinding.Option("user", rUser),
        WhereEqDatabaseOperation.Option("operation", rOp),
        WhereOrderTimestamp.Option("timestamp", rTimestamp),
        WhereOrderDatasetId.Option("dataset", rDatasetId),

        WhereModDatasetId.Option("modDatasetId", rModDatasetId),
        WhereModStepId.Option("modStepId", rModStepId),
        WhereModObservationId.Option("modObservationId", rModObservationId),
        WhereModVisitId.Option("modVisitId", rModVisitId),

        WhereModReference.Option("modReference", rModReference),
        WhereModFilename.Option("modFilename", rModFilename),
        WhereModQaState.Option("modQaState", rModQa),
        WhereModInterval.Option("modInterval", rModInterval),
        WhereModComment.Option("modComment", rModComment)

      ) =>
        (rAND, rOR, rNOT, rId, rUser, rOp, rTimestamp, rDatasetId, rModDatasetId, rModStepId, rModObservationId, rModVisitId, rModReference, rModFilename, rModQa, rModInterval, rModComment).parMapN:
          (AND, OR, NOT, id, user, op, timestamp, datasetId, modDatasetId, modStepId, modObservationId, modVisitId, modReference, modFilename, modQa, modInterval, modComment) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              id,
              user,
              op,
              timestamp,
              datasetId,
              modDatasetId,
              modStepId,
              modObservationId,
              modVisitId,
              modReference,
              modFilename,
              modQa,
              modInterval,
              modComment
            ).flatten)