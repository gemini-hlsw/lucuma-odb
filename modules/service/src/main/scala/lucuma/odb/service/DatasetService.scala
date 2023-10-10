// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.applicativeError.*
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.functor.*
import eu.timepit.refined.types.numeric.PosShort
import lucuma.core.enums.DatasetQaState
import lucuma.core.model.User
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

sealed trait DatasetService[F[_]] {

  def insertDataset(
    stepId:   Step.Id,
    filename: Dataset.Filename,
    qaState:  Option[DatasetQaState]
  )(using Transaction[F]): F[DatasetService.InsertDatasetResponse]

}

object DatasetService {

  sealed trait InsertDatasetResponse extends Product with Serializable

  sealed trait InsertDatasetFailure extends InsertDatasetResponse

  object InsertDatasetResponse {

    case class NotAuthorized(
      user: User
    ) extends InsertDatasetFailure

    case class StepNotFound(
      id: Step.Id
    ) extends InsertDatasetFailure

    case class ReusedFilename(
      filename: Dataset.Filename
    ) extends InsertDatasetFailure

    case class Success(
      datasetId: Dataset.Id,
      stepId:    Step.Id,
      index:     PosShort
    ) extends InsertDatasetResponse

  }

  def instantiate[F[_]: Concurrent](using Services[F]): DatasetService[F] =
    new DatasetService[F] with ExecutionUserCheck {

      override def insertDataset(
        stepId:   Step.Id,
        filename: Dataset.Filename,
        qaState:  Option[DatasetQaState]
      )(using Transaction[F]): F[InsertDatasetResponse] = {

        import InsertDatasetResponse.*

        val insert: F[Either[InsertDatasetFailure, (Dataset.Id, Step.Id, PosShort)]] =
          session
            .unique(Statements.InsertDataset)(stepId, filename, qaState)
            .map(_.asRight[InsertDatasetFailure])
            .recover {
              case SqlState.UniqueViolation(_)     => ReusedFilename(filename).asLeft
              case SqlState.ForeignKeyViolation(_) => StepNotFound(stepId).asLeft
              case SqlState.NotNullViolation(ex) if ex.getMessage.contains("c_observation_id") =>
                StepNotFound(stepId).asLeft
            }

        (for {
          _ <- EitherT.fromEither(checkUser(NotAuthorized.apply))
          d <- EitherT(insert).leftWiden[InsertDatasetResponse]
        } yield Success.apply.tupled(d)).merge
      }

    }

  object Statements {

    val InsertDataset: Query[(Step.Id, Dataset.Filename, Option[DatasetQaState]), (Dataset.Id, Step.Id, PosShort)] =
      sql"""
        INSERT INTO t_dataset (
          c_step_id,
          c_file_site,
          c_file_date,
          c_file_index,
          c_qa_state
        )
        SELECT
          $step_id,
          $dataset_filename,
          ${dataset_qa_state.opt}
        RETURNING
          c_dataset_id,
          c_step_id,
          c_index
      """.query(dataset_id *: step_id *: int2_pos)

  }
}
