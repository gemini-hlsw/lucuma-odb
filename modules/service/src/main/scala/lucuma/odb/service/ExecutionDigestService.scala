// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.SetupTime
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Md5Hash
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.numeric._int8
import skunk.data.Arr
import skunk.implicits.*

import scala.collection.immutable.SortedSet

sealed trait ExecutionDigestService[F[_]] {

  def selectOne(
    observationId: Observation.Id,
    hash:          Md5Hash
  )(using Transaction[F]): F[Option[ExecutionDigest]]

  def selectMany(
    input: List[(Observation.Id, Md5Hash)],
  )(using Transaction[F]): F[Map[Observation.Id, ExecutionDigest]]

  def selectAll(
    programId: Program.Id
  )(using Transaction[F]): F[Map[Observation.Id, (Md5Hash, ExecutionDigest)]]

  def insertOrUpdate(
    programId:      Program.Id,
    observationId:  Observation.Id,
    hash:           Md5Hash,
    digest:         ExecutionDigest
  )(using Transaction[F]): F[Unit]

}

object ExecutionDigestService {

  def instantiate[F[_]: Concurrent](using Services[F]): ExecutionDigestService[F] =
    new ExecutionDigestService[F] {

      override def selectOne(
        oid:  Observation.Id,
        hash: Md5Hash
      )(using Transaction[F]): F[Option[ExecutionDigest]] =
        session
          .option(Statements.SelectOneExecutionDigest)(oid)
          .map(_.collect { case (h, d) if h === hash => d })

      override def selectMany(
        input: List[(Observation.Id, Md5Hash)],
      )(using Transaction[F]): F[Map[Observation.Id, ExecutionDigest]] =
        NonEmptyList.fromList(input.map(_._1)) match
          case None => Map.empty.pure
          case Some(nel) =>
            val enc = observation_id.nel(nel)
            session
              .stream(Statements.selectManyExecutionDigest(enc))(nel, 1024)
              .compile
              .toList
              .map: list => // turn this into a lookup table
                list
                  .map:
                    case (oid, hash, digest) => oid -> (hash, digest)
                  .toMap
              .map: cached =>
                input
                  .flatMap:
                    case (oid, hash) =>
                      cached
                        .get(oid)
                        .filter(_._1 === hash)
                        .map(p => oid -> p._2)
                  .toMap

      override def selectAll(
        pid: Program.Id
      )(using Transaction[F]): F[Map[Observation.Id, (Md5Hash, ExecutionDigest)]] =
        session
          .execute(Statements.SelectAllExecutionDigest)(pid)
          .map(_.map { case (oid, hash, digest) => oid -> (hash, digest) }.toMap)

      override def insertOrUpdate(
        pid:    Program.Id,
        oid:    Observation.Id,
        hash:   Md5Hash,
        digest: ExecutionDigest
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.InsertOrUpdateExecutionDigest)(
          pid,
          oid,
          hash,
          digest.setup.full,
          digest.setup.reacquisition,
          digest.acquisition.observeClass,
          digest.acquisition.timeEstimate(ChargeClass.NonCharged),
          digest.acquisition.timeEstimate(ChargeClass.Program),
          digest.acquisition.offsets.toList,
          digest.acquisition.atomCount,
          digest.acquisition.executionState,
          digest.science.observeClass,
          digest.science.timeEstimate(ChargeClass.NonCharged),
          digest.science.timeEstimate(ChargeClass.Program),
          digest.science.offsets.toList,
          digest.science.atomCount,
          digest.science.executionState,
          hash,
          digest.setup.full,
          digest.setup.reacquisition,
          digest.acquisition.observeClass,
          digest.acquisition.timeEstimate(ChargeClass.NonCharged),
          digest.acquisition.timeEstimate(ChargeClass.Program),
          digest.acquisition.offsets.toList,
          digest.acquisition.atomCount,
          digest.acquisition.executionState,
          digest.science.observeClass,
          digest.science.timeEstimate(ChargeClass.NonCharged),
          digest.science.timeEstimate(ChargeClass.Program),
          digest.science.offsets.toList,
          digest.science.atomCount,
          digest.science.executionState
        ).void

    }

  object Statements {

    private val setup_time: Codec[SetupTime] =
      (time_span *: time_span).to[SetupTime]

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
      (obs_class *: categorized_time *: offset_array *: int4_nonneg *: execution_state).imap { case (oClass, pTime, offsets, aCount, execState) =>
        SequenceDigest(oClass, pTime, SortedSet.from(offsets), aCount, execState)
      } { sd => (
        sd.observeClass,
        sd.timeEstimate,
        sd.offsets.toList,
        sd.atomCount,
        sd.executionState
      )}

    private val execution_digest: Codec[ExecutionDigest] =
      (setup_time *: sequence_digest *: sequence_digest).to[ExecutionDigest]

    private val DigestColumns: String =
      """
        c_full_setup_time,
        c_reacq_setup_time,
        c_acq_obs_class,
        c_acq_non_charged_time,
        c_acq_program_time,
        c_acq_offsets,
        c_acq_atom_count,
        c_acq_execution_state,
        c_sci_obs_class,
        c_sci_non_charged_time,
        c_sci_program_time,
        c_sci_offsets,
        c_sci_atom_count,
        c_sci_execution_state
      """

    val SelectOneExecutionDigest: Query[Observation.Id, (Md5Hash, ExecutionDigest)] =
      sql"""
        SELECT
          c_hash,
          #$DigestColumns
        FROM t_execution_digest
        WHERE c_observation_id = $observation_id
      """.query(md5_hash *: execution_digest)

    val SelectAllExecutionDigest: Query[Program.Id, (Observation.Id, Md5Hash, ExecutionDigest)] =
      sql"""
        SELECT
          c_observation_id,
          c_hash,
          #$DigestColumns
        FROM t_execution_digest
        WHERE
          c_program_id = $program_id
      """.query(observation_id *: md5_hash *: execution_digest)

    def selectManyExecutionDigest[A <: NonEmptyList[Observation.Id]](enc: Encoder[A]): Query[A, (Observation.Id, Md5Hash, ExecutionDigest)] =
      sql"""
        SELECT
          c_observation_id,
          c_hash,
          #$DigestColumns
        FROM t_execution_digest
        WHERE
          c_observation_id in ($enc)
      """.query(observation_id *: md5_hash *: execution_digest)

    val InsertOrUpdateExecutionDigest: Command[(
      Program.Id,
      Observation.Id,
      Md5Hash,
      TimeSpan,
      TimeSpan,
      ObserveClass,
      TimeSpan,
      TimeSpan,
      List[Offset],
      NonNegInt,
      ExecutionState,
      ObserveClass,
      TimeSpan,
      TimeSpan,
      List[Offset],
      NonNegInt,
      ExecutionState,
      Md5Hash,
      TimeSpan,
      TimeSpan,
      ObserveClass,
      TimeSpan,
      TimeSpan,
      List[Offset],
      NonNegInt,
      ExecutionState,
      ObserveClass,
      TimeSpan,
      TimeSpan,
      List[Offset],
      NonNegInt,
      ExecutionState
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
          c_acq_program_time,
          c_acq_offsets,
          c_acq_atom_count,
          c_acq_execution_state,
          c_sci_obs_class,
          c_sci_non_charged_time,
          c_sci_program_time,
          c_sci_offsets,
          c_sci_atom_count,
          c_sci_execution_state
        ) SELECT
          $program_id,
          $observation_id,
          $md5_hash,
          $time_span,
          $time_span,
          $obs_class,
          $time_span,
          $time_span,
          $offset_array,
          $int4_nonneg,
          $execution_state,
          $obs_class,
          $time_span,
          $time_span,
          $offset_array,
          $int4_nonneg,
          $execution_state
        ON CONFLICT ON CONSTRAINT t_execution_digest_pkey DO UPDATE
          SET c_hash                 = $md5_hash,
              c_full_setup_time      = $time_span,
              c_reacq_setup_time     = $time_span,
              c_acq_obs_class        = $obs_class,
              c_acq_non_charged_time = $time_span,
              c_acq_program_time     = $time_span,
              c_acq_offsets          = $offset_array,
              c_acq_atom_count       = $int4_nonneg,
              c_acq_execution_state  = $execution_state,
              c_sci_obs_class        = $obs_class,
              c_sci_non_charged_time = $time_span,
              c_sci_program_time     = $time_span,
              c_sci_offsets          = $offset_array,
              c_sci_atom_count       = $int4_nonneg,
              c_sci_execution_state  = $execution_state
      """.command

  }

}
