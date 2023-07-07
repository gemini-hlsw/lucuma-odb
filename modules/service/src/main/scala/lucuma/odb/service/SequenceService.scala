// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.functor.*
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.PlannedTime
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.util.TimeSpan
import lucuma.odb.json.offset.query.given
import lucuma.odb.json.sequence.given
import lucuma.odb.json.time.query.given
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.circe.codec.json.jsonb
import skunk.codec.text.text
import skunk.implicits.*

import scala.collection.immutable.SortedSet

sealed trait SequenceService[F[_]] {

  def select[D: io.circe.Codec](
    programId:     Program.Id,
    observationId: Observation.Id,
    sequenceType:  SequenceType
  )(using Transaction[F]): F[Option[SequenceService.CachedSequence[D]]]

  def insertOrUpdate[D: io.circe.Codec](
    programId:      Program.Id,
    observationId:  Observation.Id,
    sequenceType:   SequenceType,
    cachedSequence: SequenceService.CachedSequence[D]
  )(using Transaction[F]): F[Unit]

}

object SequenceService {

  case class CachedSequence[D](
    hash:     String,
    sequence: ExecutionSequence[D],
    digest:   SequenceDigest
  )

  def instantiate[F[_]: Concurrent](using Services[F]): SequenceService[F] =
    new SequenceService[F] {
      override def select[D: io.circe.Codec](
        pid:   Program.Id,
        oid:   Observation.Id,
        sType: SequenceType
      )(using Transaction[F]): F[Option[CachedSequence[D]]] =
        session.option(Statements.SelectSequenceResult[D])(pid, oid, sType)

      override def insertOrUpdate[D: io.circe.Codec](
        pid:   Program.Id,
        oid:   Observation.Id,
        sType: SequenceType,
        seq:   CachedSequence[D]
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.InsertOrUpdateSequenceResult)(
          pid,
          oid,
          sType,
          seq.hash,
          seq.sequence,
          seq.digest.observeClass,
          seq.digest.plannedTime(ChargeClass.NonCharged),
          seq.digest.plannedTime(ChargeClass.Partner),
          seq.digest.plannedTime(ChargeClass.Program),
          seq.hash,
          seq.sequence,
          seq.digest.observeClass,
          seq.digest.plannedTime(ChargeClass.NonCharged),
          seq.digest.plannedTime(ChargeClass.Partner),
          seq.digest.plannedTime(ChargeClass.Program)
        ).void

    }

  object Statements {

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

    private val sequence_digest: Codec[SequenceDigest] =
      (obs_class *: planned_time).imap { case (oClass, pTime) =>
        SequenceDigest(oClass, pTime, SortedSet.empty)
      } { sd => (
        sd.observeClass,
        sd.plannedTime
      )}

    private def cached_sequence[D: io.circe.Codec]: Codec[CachedSequence[D]] =
      (text *: jsonb[ExecutionSequence[D]] *: sequence_digest).to[CachedSequence[D]]

    def SelectSequenceResult[D: io.circe.Codec]: Query[(
      Program.Id,
      Observation.Id,
      SequenceType
    ), CachedSequence[D]] =
      sql"""
        SELECT
          c_hash,
          c_sequence,
          c_obs_class,
          c_non_charged_time,
          c_partner_time,
          c_program_time
        FROM
          t_sequence_result
        WHERE
          c_program_id     = $program_id     AND
          c_observation_id = $observation_id AND
          c_sequence_type  = $sequence_type
      """.query(cached_sequence[D])

    def InsertOrUpdateSequenceResult[D: io.circe.Codec]: Command[(
      Program.Id,
      Observation.Id,
      SequenceType,
      String,
      ExecutionSequence[D],
      ObserveClass,
      TimeSpan,
      TimeSpan,
      TimeSpan,
      String,
      ExecutionSequence[D],
      ObserveClass,
      TimeSpan,
      TimeSpan,
      TimeSpan
    )] =
      sql"""
        INSERT INTO t_sequence_result (
          c_program_id,
          c_observation_id,
          c_sequence_type,
          c_hash,
          c_sequence,
          c_obs_class,
          c_non_charged_time,
          c_partner_time,
          c_program_time
        ) SELECT
          $program_id,
          $observation_id,
          $sequence_type,
          $text,
          ${jsonb[ExecutionSequence[D]]},
          $obs_class,
          $time_span,
          $time_span,
          $time_span
        ON CONFLICT ON CONSTRAINT t_sequence_result_pkey DO UPDATE
          SET c_hash             = $text,
              c_sequence         = ${jsonb[ExecutionSequence[D]]},
              c_obs_class        = $obs_class,
              c_non_charged_time = $time_span,
              c_partner_time     = $time_span,
              c_program_time     = $time_span
      """.command

  }

}
