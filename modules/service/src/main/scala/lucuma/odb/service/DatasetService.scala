package lucuma.odb.service

import lucuma.core.enums.DatasetQaState
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.Timestamp
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

sealed trait DatasetService[F[_]] {

  def insertDataset(
    datasetId: Dataset.Id,
    filename:  Option[Dataset.Filename],
    qaState:   Option[DatasetQaState]
  )(using Transaction[F]): F[DatasetService.InsertDatasetResponse]

}

object DatasetService {

  sealed trait InsertDatasetResponse extends Product with Serializable

  object InsertDatasetResponse {

    case class NotAuthorized(
      user: User
    ) extends InsertDatasetResponse

    case class StepNotFound(
      id: Step.Id
    ) extends InsertDatasetResponse

    case class Success(
      datasetId: Dataset.Id
    ) extends InsertDatasetResponse

  }

  object Statements {

    val InsertDataset: Command[(Dataset.Id, Dataset.Filename, DatasetQaState, Timestamp)] =
      sql"""
        INSERT INTO t_dataset (
          c_step_id,
          c_index,
          c_file_site,
          c_file_date,
          c_file_index,
          c_qa_state,
          c_timestamp
        )
        SELECT
          $dataset_id,
          $dataset_filename,
          ${dataset_qa_state.opt},
          $core_timestamp
      """.command

  }
}
