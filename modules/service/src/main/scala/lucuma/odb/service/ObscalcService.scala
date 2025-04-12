// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import io.circe.syntax.*
import lucuma.core.enums.ChargeClass
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.SetupTime
import lucuma.core.util.Timestamp
import lucuma.odb.data.Obscalc
import lucuma.odb.logic.Generator
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.categorized_time
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.execution_state
import lucuma.odb.util.Codecs.int4_nonneg
import lucuma.odb.util.Codecs.nel
import lucuma.odb.util.Codecs.obs_class
import lucuma.odb.util.Codecs.obscalc_state
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.program_id
import lucuma.odb.util.Codecs.time_span
import skunk.*
import skunk.circe.codec.json.*
import skunk.codec.numeric._int8
import skunk.codec.numeric.int4
import skunk.data.Arr
import skunk.implicits.*

import scala.collection.immutable.SortedSet

sealed trait ObscalcService[F[_]]:

  def selectOne(
    observationId: Observation.Id
  )(using Transaction[F]): F[Option[Obscalc]]

  def selectMany(
    observationIds: List[Observation.Id]
  )(using Transaction[F]): F[Map[Observation.Id, Obscalc]]

  def selectProgram(
    programId: Program.Id
  )(using Transaction[F]): F[Map[Observation.Id, Obscalc]]

  def resetCalculating(using Transaction[F]): F[Unit]

  def loadPendingCalc(
    max: Int
  )(using Transaction[F]): F[List[Obscalc.PendingCalc]]

/*
  def calculateResult(
    pending: Obscalc.PendingCalc
  ): F[Obscalc.Result]
*/
  def storeResult(
    pending: Obscalc.PendingCalc,
    result:  Obscalc.Result
  )(using Transaction[F]): F[Unit]

object ObscalcService:

  def instantiate[F[_]: Concurrent](
    itc: ItcService[F],
    gen: Generator[F]
  )(using Services[F]): ObscalcService[F] =

    new ObscalcService[F]:
      override def selectOne(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[Obscalc]] =
        session.option(Statements.SelectOne)(observationId)

      override def selectMany(
        observationIds: List[Observation.Id]
      )(using Transaction[F]): F[Map[Observation.Id, Obscalc]] =
        NonEmptyList.fromList(observationIds) match
          case None      => Map.empty.pure
          case Some(nel) =>
            val enc = observation_id.nel(nel)
            session
              .stream(Statements.selectMany(enc))(nel, 1024)
              .compile
              .toList
              .map(_.fproductLeft(_.observationId).toMap)

      override def selectProgram(
        programId: Program.Id
      )(using Transaction[F]): F[Map[Observation.Id, Obscalc]] =
        session
          .execute(Statements.SelectProgram)(programId)
          .map(_.fproductLeft(_.observationId).toMap)

      override def resetCalculating(using Transaction[F]): F[Unit] =
        session.execute(Statements.ResetCalculating).void

      override def loadPendingCalc(
        max: Int
      )(using Transaction[F]): F[List[Obscalc.PendingCalc]] =
        session.execute(Statements.LoadPendingCalc)(max)

/*
      override def calculateResult(
        pending: Obscalc.PendingCalc
      )(using NoTransaction[F]): F[Obscalc.Result] =
        for
          r <- itc.lookup(pending.programId, pending.observationId)
          d <- gen.digest()
        yield r
*/

      override def storeResult(
        pending: Obscalc.PendingCalc,
        result:  Obscalc.Result
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.StoreResults)(pending, result).void

  object Statements:
    private val pending_obscalc: Codec[Obscalc.PendingCalc] =
      (program_id *: observation_id *: core_timestamp).to[Obscalc.PendingCalc]

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

    private val itc_result: Codec[ItcService.Result] =
      jsonb.eimap(
        _.as[ItcService.Result].leftMap(f => s"Could not decode ItcService.Result: ${f.message}")
      )(_.asJson)

    private val obscalc_result: Codec[Obscalc.Result] =
      (itc_result *: execution_digest.opt).to[Obscalc.Result]

    private val obscalc: Codec[Obscalc] =
      (observation_id *: obscalc_state *: core_timestamp *: core_timestamp *: obscalc_result).to[Obscalc]

    private def obscalcColumns(prefix: Option[String] = None): String =
      List(
        "c_observation_id",
        "c_obscalc_state",
        "c_last_invalidation",
        "c_last_update",
        "c_itc_result",
        "c_full_setup_time",
        "c_reacq_setup_time",
        "c_acq_obs_class",
        "c_acq_non_charged_time",
        "c_acq_program_time",
        "c_acq_offsets",
        "c_acq_atom_count",
        "c_acq_execution_state",
        "c_sci_obs_class",
        "c_sci_non_charged_time",
        "c_sci_program_time",
        "c_sci_offsets",
        "c_sci_atom_count",
        "c_sci_execution_state"
      ).map(col => prefix.fold(col)(p => s"$p.$col"))
       .mkString("", ",\n", "\n")

    val SelectOne: Query[Observation.Id, Obscalc] =
      sql"""
        SELECT
          #${obscalcColumns()}
        FROM t_obscalc
        WHERE c_observation_id = $observation_id
      """.query(obscalc)

    def selectMany[A <: NonEmptyList[Observation.Id]](enc: Encoder[A]): Query[A, Obscalc] =
      sql"""
        SELECT
          #${obscalcColumns()}
        FROM t_obscalc
        WHERE c_observation_id in ($enc)
      """.query(obscalc)

    val SelectProgram: Query[Program.Id, Obscalc] =
      sql"""
        SELECT
          #${obscalcColumns("c".some)}
        FROM t_obscalc c
        INNER JOIN t_observation o USING (c_observation_id)
        WHERE o.c_program_id = $program_id
      """.query(obscalc)

    val ResetCalculating: Command[Void] =
      sql"""
        UPDATE t_obscalc c
        SET c_obscalc_state = 'pending'
        WHERE c_obscalc_state = 'calculating'
      """.command

    val LoadPendingCalc: Query[Int, Obscalc.PendingCalc] =
      sql"""
        WITH tasks AS (
          SELECT o.c_program_id, o.c_observation_id
          FROM t_obscalc c
          INNER JOIN t_observation o USING (c_observation_id)
          WHERE c.c_obscalc_state = 'pending'
          ORDER BY c.c_last_invalidation LIMIT $int4
          FOR UPDATE SKIP LOCKED
        )
        UPDATE t_obscalc c
        SET c_obscalc_state = 'calculating'
        FROM tasks
        WHERE c.c_observation_id = tasks.c_observation_id
        RETURNING tasks.c_program_id, c.c_observation_id, c_last_invalidation
      """.query(pending_obscalc)

    val StoreResults: Command[(
      Obscalc.PendingCalc,
      Obscalc.Result
    )] =
      sql"""
        UPDATE t_obscalc
        SET
          c_obscalc_state        = CASE
                                     WHEN c_last_invalidation = $core_timestamp THEN 'ready'
                                     ELSE 'pending'
                                   END,
          c_last_update          = now(),
          c_itc_result           = $jsonb,
          c_full_setup_time      = ${time_span.opt},
          c_reacq_setup_time     = ${time_span.opt},
          c_acq_obs_class        = ${obs_class.opt},
          c_acq_non_charged_time = ${time_span.opt},
          c_acq_program_time     = ${time_span.opt},
          c_acq_offsets          = ${offset_array.opt},
          c_acq_atom_count       = ${int4_nonneg.opt},
          c_acq_execution_state  = ${execution_state.opt},
          c_sci_obs_class        = ${obs_class.opt},
          c_sci_non_charged_time = ${time_span.opt},
          c_sci_program_time     = ${time_span.opt},
          c_sci_offsets          = ${offset_array.opt},
          c_sci_atom_count       = ${int4_nonneg.opt},
          c_sci_execution_state  = ${execution_state.opt}
        WHERE c_observation_id = $observation_id
      """.command
         .contramap: (p, r) =>
           (
             p.lastInvalidation,
             r.itc.asJson,
             r.digest.map(_.setup.full),
             r.digest.map(_.setup.reacquisition),
             r.digest.map(_.acquisition.observeClass),
             r.digest.map(_.acquisition.timeEstimate(ChargeClass.NonCharged)),
             r.digest.map(_.acquisition.timeEstimate(ChargeClass.Program)),
             r.digest.map(_.acquisition.offsets.toList),
             r.digest.map(_.acquisition.atomCount),
             r.digest.map(_.acquisition.executionState),
             r.digest.map(_.science.observeClass),
             r.digest.map(_.science.timeEstimate(ChargeClass.NonCharged)),
             r.digest.map(_.science.timeEstimate(ChargeClass.Program)),
             r.digest.map(_.science.offsets.toList),
             r.digest.map(_.science.atomCount),
             r.digest.map(_.science.executionState),
             p.observationId
           )