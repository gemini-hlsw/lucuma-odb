// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.odb.graphql.binding.*

case class RecordDatasetInput(
  stepId:   Step.Id,
  filename: Dataset.Filename
)

object RecordDatasetInput {

  val Binding: Matcher[RecordDatasetInput] =
    ObjectFieldsBinding.rmap {
      case List(
        StepIdBinding("stepId", rStepId),
        DatasetFilenameBinding("filename", rFilename)
      ) =>
        (rStepId, rFilename).parMapN { (sid, filename) =>
          RecordDatasetInput(sid, filename)
        }
    }

}

