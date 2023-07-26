// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.PlannedTime
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.SetupTime
import lucuma.core.util.TimeSpan
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.numeric._int8
import skunk.codec.text.text
import skunk.data.Arr
import skunk.implicits.*

import scala.collection.immutable.SortedSet

sealed trait ExecutionDigestService[F[_]] {

  def select(
    programId:     Program.Id,
    observationId: Observation.Id,
    hash:          String
  )(using Transaction[F]): F[Option[ExecutionDigest]]

  def insertOrUpdate(
    programId:      Program.Id,
    observationId:  Observation.Id,
    hash:           String,
    digest:         ExecutionDigest
  )(using Transaction[F]): F[Unit]

}

object ExecutionDigestService {

  def instantiate[F[_]: Concurrent](using Services[F]): ExecutionDigestService[F] =
    new ExecutionDigestService[F] {
      override def select(
        pid:  Program.Id,
        oid:  Observation.Id,
        hash: String
      )(using Transaction[F]): F[Option[ExecutionDigest]] =
        session
          .option(Statements.SelectExecutionDigest)(pid, oid)
          .map(_.collect { case (h, d) if h === hash => d })

      override def insertOrUpdate(
        pid:    Program.Id,
        oid:    Observation.Id,
        hash:   String,
        digest: ExecutionDigest
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.InsertOrUpdateExecutionDigest)(
          pid,
          oid,
          hash,
          digest.setup.full,
          digest.setup.reacquisition,
          digest.acquisition.observeClass,
          digest.acquisition.plannedTime(ChargeClass.NonCharged),
          digest.acquisition.plannedTime(ChargeClass.Partner),
          digest.acquisition.plannedTime(ChargeClass.Program),
          digest.acquisition.offsets.toList,
          digest.acquisition.atomCount,
          digest.science.observeClass,
          digest.science.plannedTime(ChargeClass.NonCharged),
          digest.science.plannedTime(ChargeClass.Partner),
          digest.science.plannedTime(ChargeClass.Program),
          digest.science.offsets.toList,
          digest.science.atomCount,
          hash,
          digest.setup.full,
          digest.setup.reacquisition,
          digest.acquisition.observeClass,
          digest.acquisition.plannedTime(ChargeClass.NonCharged),
          digest.acquisition.plannedTime(ChargeClass.Partner),
          digest.acquisition.plannedTime(ChargeClass.Program),
          digest.acquisition.offsets.toList,
          digest.acquisition.atomCount,
          digest.science.observeClass,
          digest.science.plannedTime(ChargeClass.NonCharged),
          digest.science.plannedTime(ChargeClass.Partner),
          digest.science.plannedTime(ChargeClass.Program),
          digest.science.offsets.toList,
          digest.science.atomCount
        ).void

    }

  object Statements {

    private val setup_time: Codec[SetupTime] =
      (time_span *: time_span).to[SetupTime]

    private val planned_time: Codec[PlannedTime] =
      (time_span *: time_span *: time_span).imap {
        case (non_charged, partner_time, program_time) =>
          PlannedTime(
            ChargeClass.NonCharged -> non_charged,
            ChargeClass.Partner    -> partner_time,
            ChargeClass.Program    -> program_time
          )
      } { pt =>
        (pt(ChargeClass.NonCharged), pt(ChargeClass.Partner), pt(ChargeClass.Program))
      }

    private val offset_array: Codec[List[Offset]] =
      _int8.eimap { arr =>
        val len = arr.size / 2
        if (arr.size % 2 =!= 0) "Expected an even number of offset coordinates".asLeft
        else arr.reshape(len, 2).fold("Quite unexpectedly, cannot reshape offsets to an Nx2 array".asLeft[List[Offset]]) { arr =>
          Either.fromOption(
            (0 until len).toList.traverse { index =>
              (arr.get(index, 0), arr.get(index, 1)).mapN { (p, q) =>
                Offset.signedMicroarcseconds.reverseGet((p, q))
              }
            },
            "Invalid offset array"
          )
        }
      } { offsets =>
        Arr
          .fromFoldable(offsets.flatMap(o => Offset.signedMicroarcseconds.get(o).toList))
          .reshape(offsets.size, 2)
          .get
      }

    private val sequence_digest: Codec[SequenceDigest] =
      (obs_class *: planned_time *: offset_array *: int4_nonneg).imap { case (oClass, pTime, offsets, aCount) =>
        SequenceDigest(oClass, pTime, SortedSet.from(offsets), aCount)
      } { sd => (
        sd.observeClass,
        sd.plannedTime,
        sd.offsets.toList,
        sd.atomCount
      )}

    private val execution_digest: Codec[ExecutionDigest] =
      (setup_time *: sequence_digest *: sequence_digest).to[ExecutionDigest]

    def SelectExecutionDigest: Query[(Program.Id, Observation.Id), (String, ExecutionDigest)] =
      sql"""
        SELECT
          c_hash,
          c_full_setup_time,
          c_reacq_setup_time,
          c_acq_obs_class,
          c_acq_non_charged_time,
          c_acq_partner_time,
          c_acq_program_time,
          c_acq_offsets,
          c_acq_atom_count,
          c_sci_obs_class,
          c_sci_non_charged_time,
          c_sci_partner_time,
          c_sci_program_time,
          c_sci_offsets,
          c_sci_atom_count
        FROM
          t_execution_digest
        WHERE
          c_program_id     = $program_id     AND
          c_observation_id = $observation_id
      """.query(text *: execution_digest)

    def InsertOrUpdateExecutionDigest: Command[(
      Program.Id,
      Observation.Id,
      String,
      TimeSpan,
      TimeSpan,
      ObserveClass,
      TimeSpan,
      TimeSpan,
      TimeSpan,
      List[Offset],
      NonNegInt,
      ObserveClass,
      TimeSpan,
      TimeSpan,
      TimeSpan,
      List[Offset],
      NonNegInt,
      String,
      TimeSpan,
      TimeSpan,
      ObserveClass,
      TimeSpan,
      TimeSpan,
      TimeSpan,
      List[Offset],
      NonNegInt,
      ObserveClass,
      TimeSpan,
      TimeSpan,
      TimeSpan,
      List[Offset],
      NonNegInt
    )] =
      sql"""
        INSERT INTO t_execution_digest (
          c_program_id,
          c_observation_id,
          c_hash,
          c_full_setup_time,
          c_reacq_setup_time,
          c_acq_obs_class,
          c_acq_non_charged_time,
          c_acq_partner_time,
          c_acq_program_time,
          c_acq_offsets,
          c_acq_atom_count,
          c_sci_obs_class,
          c_sci_non_charged_time,
          c_sci_partner_time,
          c_sci_program_time,
          c_sci_offsets,
          c_sci_atom_count
        ) SELECT
          $program_id,
          $observation_id,
          $text,
          $time_span,
          $time_span,
          $obs_class,
          $time_span,
          $time_span,
          $time_span,
          $offset_array,
          $int4_nonneg,
          $obs_class,
          $time_span,
          $time_span,
          $time_span,
          $offset_array,
          $int4_nonneg
        ON CONFLICT ON CONSTRAINT t_execution_digest_pkey DO UPDATE
          SET c_hash                 = $text,
              c_full_setup_time      = $time_span,
              c_reacq_setup_time     = $time_span,
              c_acq_obs_class        = $obs_class,
              c_acq_non_charged_time = $time_span,
              c_acq_partner_time     = $time_span,
              c_acq_program_time     = $time_span,
              c_acq_offsets          = $offset_array,
              c_acq_atom_count       = $int4_nonneg,
              c_sci_obs_class        = $obs_class,
              c_sci_non_charged_time = $time_span,
              c_sci_partner_time     = $time_span,
              c_sci_program_time     = $time_span,
              c_sci_offsets          = $offset_array,
              c_sci_atom_count       = $int4_nonneg
      """.command

  }

}
