// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.DatasetStage
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.IdempotencyKey
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.*

case class AddDatasetEventInput(
  datasetId:      Dataset.Id,
  datasetStage:   DatasetStage,
  time:           Option[Timestamp],
  idempotencyKey: Option[IdempotencyKey]
)

object AddDatasetEventInput:

  val Binding: Matcher[AddDatasetEventInput] =
    ObjectFieldsBinding.rmap:
      case List(
        DatasetIdBinding("datasetId", rDatasetId),
        DatasetStageBinding("datasetStage", rDatasetStage),
        TimestampBinding.Option("time", rTime),
        IdempotencyKeyBinding.Option("idempotencyKey", rIdm)
      ) =>
        (rDatasetId, rDatasetStage, rTime, rIdm).parMapN: (did, stage, time, idm) =>
          AddDatasetEventInput(did, stage, time, idm)