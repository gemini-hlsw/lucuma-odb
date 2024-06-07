// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.DatasetQaState
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.odb.graphql.binding.*

case class RecordDatasetInput(
  stepId:   Step.Id,
  filename: Dataset.Filename,
  qaState:  Option[DatasetQaState],
  comment:  Option[NonEmptyString]
)

object RecordDatasetInput {

  val Binding: Matcher[RecordDatasetInput] =
    ObjectFieldsBinding.rmap {
      case List(
        StepIdBinding("stepId", rStepId),
        DatasetFilenameBinding("filename", rFilename),
        DatasetQaStateBinding.Option("qaState", rQaState),
        NonEmptyStringBinding.Option("comment", rComment)
      ) =>
        (rStepId, rFilename, rQaState, rComment).parMapN { (sid, filename, qaState, comment) =>
          RecordDatasetInput(sid, filename, qaState, comment)
        }
    }

}

