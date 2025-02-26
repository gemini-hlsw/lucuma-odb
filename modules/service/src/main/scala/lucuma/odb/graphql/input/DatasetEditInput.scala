// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.apply.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.Dataset
import lucuma.odb.graphql.binding.*

case class DatasetEditInput(
  datasetId:     Option[Dataset.Id],
  observationId: Option[Observation.Id],
  programId:     Option[Program.Id],
  isWritten:     Option[Boolean]
)

object DatasetEditInput:

  val Binding = ObjectFieldsBinding.rmap:
    case List(
      DatasetIdBinding.Option("datasetId", rDatasetId),
      ObservationIdBinding.Option("observationId", rObservationId),
      ProgramIdBinding.Option("programId", rProgramId),
      BooleanBinding.Option("isWritten", rIsWritten)
    ) => (rDatasetId, rObservationId, rProgramId, rIsWritten).mapN(apply)