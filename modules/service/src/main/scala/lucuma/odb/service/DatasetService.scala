// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicativeError.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.DatasetReference
import lucuma.core.util.Timestamp
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.DatasetPropertiesInput
import lucuma.odb.graphql.input.RecordDatasetInput
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
    input: RecordDatasetInput
  )(using Transaction[F], Services.ServiceAccess): F[Result[Dataset.Id]]

  def updateDatasets(
    SET:   DatasetPropertiesInput,
    which: AppliedFragment
  )(using Transaction[F], Services.StaffAccess): F[List[Dataset.Id]]

  def setStartTime(
    datasetId: Dataset.Id,
    time:      Timestamp
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def setEndTime(
    datasetId: Dataset.Id,
    time:      Timestamp
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def hasDatasets(
    visitId: Visit.Id
  )(using Transaction[F]): F[Boolean]

  def selectDatasetsWithQaFailures(
    visitId: Visit.Id
  )(using Transaction[F]): F[List[(Atom.Id, List[Dataset.Id])]]
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
        input: RecordDatasetInput
      )(using Transaction[F], Services.ServiceAccess): F[Result[Dataset.Id]] = {

        def stepNotFound: Result[Dataset.Id] =
          OdbError.InvalidStep(input.stepId, Some(s"Step id '${input.stepId}' not found")).asFailure

        val insert: F[Result[Dataset.Id]] =
          session
            .unique(Statements.InsertDataset)(input)
            .map(_.success)
            .recover {
              case SqlState.UniqueViolation(_)     => OdbError.InvalidFilename(input.filename, Some(s"The filename '${input.filename.format}' is already assigned")).asFailure
              case SqlState.ForeignKeyViolation(_) => stepNotFound
              case SqlState.NotNullViolation(ex) if ex.getMessage.contains("c_observation_id") => stepNotFound
            }

        ResultT(insert).value
      }

      override def updateDatasets(
        SET:   DatasetPropertiesInput,
        which: AppliedFragment
      )(using Transaction[F], Services.StaffAccess): F[List[Dataset.Id]] =
        Statements.UpdateDatasets(SET, which).toList.flatTraverse { af =>
          session.prepareR(af.fragment.query(dataset_id)).use { pq =>
            pq.stream(af.argument, chunkSize = 1024).compile.toList
          }
        }

      override def setStartTime(
        datasetId: Dataset.Id,
        time:      Timestamp
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        session.execute(Statements.SetStartTime)(time, datasetId).void

      override def setEndTime(
        datasetId: Dataset.Id,
        time:      Timestamp
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
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
  }

  object Statements {

    val selectDid: Query[DatasetReference, Dataset.Id] =
      sql"""
        SELECT c_dataset_id
          FROM t_dataset
         WHERE c_dataset_reference = $dataset_reference
      """.query(dataset_id)

    val InsertDataset: Query[RecordDatasetInput, Dataset.Id] =
      sql"""
        INSERT INTO t_dataset (
          c_step_id,
          c_file_site,
          c_file_date,
          c_file_index,
          c_qa_state,
          c_comment,
          c_idempotency_key
        )
        SELECT
          $step_id,
          $dataset_filename,
          ${dataset_qa_state.opt},
          ${text_nonempty.opt},
          ${idempotency_key.opt}
        ON CONFLICT (c_idempotency_key) DO UPDATE
          SET c_idempotency_key = EXCLUDED.c_idempotency_key
        RETURNING
          c_dataset_id
      """.query(dataset_id).contramap { input => (
        input.stepId,
        input.filename,
        input.qaState,
        input.comment,
        input.idempotencyKey
      )}

    def UpdateDatasets(
      SET:   DatasetPropertiesInput,
      which: AppliedFragment
    ): Option[AppliedFragment] = {
      val upQaState = sql"c_qa_state = ${dataset_qa_state.opt}"
      val upComment = sql"c_comment = ${text_nonempty.opt}"

      val updates = NonEmptyList.fromList(List(
        SET.qaState.foldPresent(upQaState),
        SET.comment.foldPresent(upComment)
      ).flatten)

      updates.map { ups =>
        void"UPDATE t_dataset SET " |+| ups.intercalate(void", ") |+|
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
  }
}
