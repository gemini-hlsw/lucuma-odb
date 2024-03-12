// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.applicativeError.*
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
import lucuma.core.enums.DatasetQaState
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.Timestamp
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.DatasetPropertiesInput
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.implicits.*

import Services.Syntax.*

sealed trait DatasetService[F[_]] {

  def insertDataset(
    stepId:   Step.Id,
    filename: Dataset.Filename,
    qaState:  Option[DatasetQaState]
  )(using Transaction[F]): F[Result[Dataset.Id]]

  def updateDatasets(
    SET:   DatasetPropertiesInput,
    which: AppliedFragment
  )(using Transaction[F]): F[List[Dataset.Id]]

  def setStartTime(
    datasetId: Dataset.Id,
    time:      Timestamp
  )(using Transaction[F]): F[Unit]

  def setEndTime(
    datasetId: Dataset.Id,
    time:      Timestamp
  )(using Transaction[F]): F[Unit]

  def hasDatasets(
    visitId: Visit.Id
  )(using Transaction[F]): F[Boolean]

  def selectDatasetsWithQaFailures(
    visitId: Visit.Id
  )(using Transaction[F]): F[List[(Atom.Id, List[Dataset.Id])]]

  def selectStepQaState(
    stepId: Step.Id
  )(using Transaction[F]): F[Option[DatasetQaState]]
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
      datasetId:     Dataset.Id,
      stepId:        Step.Id,
      exposureIndex: PosInt
    ) extends InsertDatasetResponse

  }

  def instantiate[F[_]: Concurrent](using Services[F]): DatasetService[F] =
    new DatasetService[F] with ExecutionUserCheck {

      def insertDatasetImpl(
        stepId:   Step.Id,
        filename: Dataset.Filename,
        qaState:  Option[DatasetQaState]
      )(using Transaction[F]): F[InsertDatasetResponse] = {

        import InsertDatasetResponse.*

        val insert: F[Either[InsertDatasetFailure, (Dataset.Id, Step.Id, PosInt)]] =
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

      def insertDataset(
        stepId:   Step.Id,
        filename: Dataset.Filename,
        qaState:  Option[DatasetQaState]
      )(using Transaction[F]): F[Result[Dataset.Id]] = 
        import InsertDatasetResponse.*
        insertDatasetImpl(stepId, filename, qaState).map:
          case NotAuthorized(user)      => OdbError.NotAuthorized(user.id).asFailure
          case ReusedFilename(filename) => OdbError.InvalidFilename(filename, Some(s"The filename '${filename.format}' is already assigned")).asFailure
          case StepNotFound(id)         => OdbError.InvalidStep(id, Some(s"Step id '$id' not found")).asFailure
          case Success(did, _, _)       => Result(did)

      override def updateDatasets(
        SET:   DatasetPropertiesInput,
        which: AppliedFragment
      )(using Transaction[F]): F[List[Dataset.Id]] =
        Statements.UpdateDatasets(SET, which).toList.flatTraverse { af =>
          session.prepareR(af.fragment.query(dataset_id)).use { pq =>
            pq.stream(af.argument, chunkSize = 1024).compile.toList
          }
        }

      override def setStartTime(
        datasetId: Dataset.Id,
        time:      Timestamp
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.SetStartTime)(time, datasetId).void

      override def setEndTime(
        datasetId: Dataset.Id,
        time:      Timestamp
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.SetEndTime)(time, datasetId).void

      override def hasDatasets(
        visitId: Visit.Id
      )(using Transaction[F]): F[Boolean] =
        session.unique(Statements.HasDatasets)(visitId)

      override def selectDatasetsWithQaFailures(
        visitId: Visit.Id
      )(using Transaction[F]): F[List[(Atom.Id, List[Dataset.Id])]] =
        session.execute(Statements.SelectQaFailures)(visitId).map {
          _.groupBy(_._1).view.mapValues(_.unzip._2).toList
        }

      override def selectStepQaState(
        stepId: Step.Id
      )(using Transaction[F]): F[Option[DatasetQaState]] =
        session.unique(Statements.SelectStepQaState)(stepId)

  }

  object Statements {

    val InsertDataset: Query[(Step.Id, Dataset.Filename, Option[DatasetQaState]), (Dataset.Id, Step.Id, PosInt)] =
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
          c_exposure_index
      """.query(dataset_id *: step_id *: int4_pos)

    def UpdateDatasets(
      SET:   DatasetPropertiesInput,
      which: AppliedFragment
    ): Option[AppliedFragment] = {
      val upQaState = sql"c_qa_state = ${dataset_qa_state.opt}"

      val update = SET.qaState match {
        case Nullable.Absent      => None
        case Nullable.Null        => Some(upQaState(None))
        case Nullable.NonNull(qa) => Some(upQaState(Some(qa)))
      }

      update.map { up =>
        void"UPDATE t_dataset SET " |+| up |+|
          void" WHERE c_dataset_id IN (" |+| which |+| void")" |+|
          void" RETURNING c_dataset_id"
      }
    }

    val SetStartTime: Command[(Timestamp, Dataset.Id)] =
      sql"""
        UPDATE t_dataset
           SET c_start_time = $core_timestamp,
               c_end_time   = NULL
         WHERE c_dataset_id = $dataset_id
      """.command

    val SetEndTime: Command[(Timestamp, Dataset.Id)] =
      sql"""
        UPDATE t_dataset
           SET c_end_time   = $core_timestamp
         WHERE c_dataset_id = $dataset_id
      """.command

    val HasDatasets: Query[Visit.Id, Boolean] =
      sql"""
        SELECT EXISTS (
            SELECT 1
            FROM t_dataset
            WHERE c_visit_id = $visit_id
        )
      """.query(bool)

    val SelectQaFailures: Query[Visit.Id, (Atom.Id, Dataset.Id)] =
      sql"""
        SELECT s.c_atom_id,
               d.c_dataset_id
          FROM t_dataset d
    INNER JOIN t_step_record s
            ON s.c_step_id = d.c_step_id
         WHERE (d.c_qa_state = 'Fail' OR d.c_qa_state = 'Usable')
           AND d.c_visit_id = $visit_id
      """.query(atom_id *: dataset_id)

    val SelectStepQaState: Query[Step.Id, Option[DatasetQaState]] =
      sql"""
        SELECT MAX(c_qa_state)
          FROM t_dataset
         WHERE c_step_id = $step_id
      """.query(dataset_qa_state.opt)
  }
}
