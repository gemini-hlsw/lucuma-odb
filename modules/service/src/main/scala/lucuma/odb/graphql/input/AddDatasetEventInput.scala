// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.DatasetStage
import lucuma.core.model.sequence.Dataset
import lucuma.odb.graphql.binding.*

case class AddDatasetEventInput(
  datasetId:    Dataset.Id,
  datasetStage: DatasetStage
)

object AddDatasetEventInput {

  val Binding: Matcher[AddDatasetEventInput] =
    ObjectFieldsBinding.rmap {
      case List(
        DatasetIdBinding("datasetId", rDatasetId),
        DatasetStageBinding("datasetStage", rDatasetStage)
      ) =>
        (rDatasetId, rDatasetStage).parMapN { (did, stage) =>
          AddDatasetEventInput(did, stage)
        }
    }

}

