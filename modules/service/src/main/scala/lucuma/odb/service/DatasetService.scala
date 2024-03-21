// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.applicativeError.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.DatasetReference
import lucuma.core.model.sequence.Step
import lucuma.core.util.Timestamp
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.DatasetPropertiesInput
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.implicits.*

import Services.Syntax.*

sealed trait DatasetService[F[_]] {

  /**
   * Finds the dataset id consistent with the given ids (if any).
   */
  def resolveDid(
    did: Option[Dataset.Id],
    ref: Option[DatasetReference]
  ): F[Result[Dataset.Id]]

  def insertDataset(
    stepId:   Step.Id,
    filename: Dataset.Filename,
    qaState:  Option[DatasetQaState]
  )(using Transaction[F], Services.ServiceAccess): F[Result[Dataset.Id]]

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

  def instantiate[F[_]: Concurrent](using Services[F]): DatasetService[F] =
    new DatasetService[F] {

      val resolver = new IdResolver("dataset", Statements.selectDid, _.label)

      override def resolveDid(
        did: Option[Dataset.Id],
        ref: Option[DatasetReference]
      ): F[Result[Dataset.Id]] =
        resolver.resolve(did, ref)

      override def insertDataset(
        stepId:   Step.Id,
        filename: Dataset.Filename,
        qaState:  Option[DatasetQaState]
      )(using Transaction[F], Services.ServiceAccess): F[Result[Dataset.Id]] = {

        def stepNotFound: Result[Dataset.Id] =
          OdbError.InvalidStep(stepId, Some(s"Step id '$stepId' not found")).asFailure

        val insert: F[Result[Dataset.Id]] =
          session
            .unique(Statements.InsertDataset)(stepId, filename, qaState)
            .map(_.success)
            .recover {
              case SqlState.UniqueViolation(_)     => OdbError.InvalidFilename(filename, Some(s"The filename '${filename.format}' is already assigned")).asFailure
              case SqlState.ForeignKeyViolation(_) => stepNotFound
              case SqlState.NotNullViolation(ex) if ex.getMessage.contains("c_observation_id") => stepNotFound
            }

        ResultT(insert).value
      }

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

    val selectDid: Query[DatasetReference, Dataset.Id] =
      sql"""
        SELECT c_dataset_id
          FROM t_dataset
         WHERE c_dataset_reference = $dataset_reference
      """.query(dataset_id)

    val InsertDataset: Query[(Step.Id, Dataset.Filename, Option[DatasetQaState]), Dataset.Id] =
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
          c_dataset_id
      """.query(dataset_id)

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
