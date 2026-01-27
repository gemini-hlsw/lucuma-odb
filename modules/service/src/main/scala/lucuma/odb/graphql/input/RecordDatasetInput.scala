// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.DatasetQaState
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.IdempotencyKey
import lucuma.odb.graphql.binding.*

case class RecordDatasetInput(
  stepId:         Step.Id,
  visitId:        Visit.Id,
  filename:       Dataset.Filename,
  qaState:        Option[DatasetQaState],
  comment:        Option[NonEmptyString],
  idempotencyKey: Option[IdempotencyKey]
)

object RecordDatasetInput:

  val Binding: Matcher[RecordDatasetInput] =
    ObjectFieldsBinding.rmap:
      case List(
        StepIdBinding("stepId", rStepId),
        VisitIdBinding("visitId", rVisitId),
        DatasetFilenameBinding("filename", rFilename),
        DatasetQaStateBinding.Option("qaState", rQaState),
        NonEmptyStringBinding.Option("comment", rComment),
        IdempotencyKeyBinding.Option("idempotencyKey", rIdm)
      ) =>
        (rStepId, rVisitId, rFilename, rQaState, rComment, rIdm).parMapN:
          (sid, vid, filename, qaState, comment, idm) =>
            RecordDatasetInput(sid, vid, filename, qaState, comment, idm)