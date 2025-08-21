// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.DatasetStage
import lucuma.core.model.Client
import lucuma.core.model.sequence.Dataset
import lucuma.odb.graphql.binding.*

case class AddDatasetEventInput(
  datasetId:    Dataset.Id,
  datasetStage: DatasetStage,
  clientId:     Option[Client.Id]
)

object AddDatasetEventInput:

  val Binding: Matcher[AddDatasetEventInput] =
    ObjectFieldsBinding.rmap:
      case List(
        DatasetIdBinding("datasetId", rDatasetId),
        DatasetStageBinding("datasetStage", rDatasetStage),
        ClientIdBinding.Option("clientId", rCid)
      ) =>
        (rDatasetId, rDatasetStage, rCid).parMapN: (did, stage, cid) =>
          AddDatasetEventInput(did, stage, cid)