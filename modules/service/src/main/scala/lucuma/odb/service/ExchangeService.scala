// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.implicits.*
import grackle.Result
import lucuma.core.model.Observation
import lucuma.odb.graphql.input.ExchangeInput
import lucuma.odb.sequence.exchange.Config
import lucuma.odb.service.Services.Syntax.session
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.data.Completion
import skunk.syntax.all.*

trait ExchangeService[F[_]]:

  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config]]

  def insert(
    input: ExchangeInput.Create,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def update(
    SET: ExchangeInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def clone(
    originalId: Observation.Id,
    newId: Observation.Id
  )(using Transaction[F]): F[Unit]

object ExchangeService:

  def instantiate[F[_]: Concurrent](using Services[F]): ExchangeService[F] =
    new ExchangeService[F]:

      override def select(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        NonEmptyList.fromList(which) match
          case None      => Map.empty.pure
          case Some(nel) =>
            session.prepareR(Statements.select(nel)).use: pq =>
              pq.stream(nel, 1024).compile.toList.map(_.toMap)

      override def insert(
        input: ExchangeInput.Create,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        session.prepareR(Statements.Insert).use: ps =>
          which
            .traverse: oid =>
              ps.execute(oid, input).map:
                case Completion.Insert(1) => Result.unit
                case other => Result.failure(s"Expected single insert, got $other.")
            .map(_.sequenceVoid)

      override def delete(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        NonEmptyList.fromList(which) match
          case None      => ().pure
          case Some(nel) =>
            session.prepareR(Statements.delete(nel)).use: ps =>
              ps.execute(nel).void

      override def update(
        SET: ExchangeInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] = {

        // When an instrument is provided we set the matching column and null
        // the other, keeping the exactly-one-non-null invariant.
        val update: Option[AppliedFragment] =
          NonEmptyList
            .fromList:
              List(
                SET.instrument.map:
                  case Left(k)  => sql"c_keck_instrument = ${keck_instrument}, c_subaru_instrument = ${subaru_instrument.opt}"(k, none)
                  case Right(s) => sql"c_keck_instrument = ${keck_instrument.opt}, c_subaru_instrument = ${subaru_instrument}"(none, s),
                SET.totalRequestTime.map(sql"c_total_request_time = $time_span")
              ).flatten
            .map(_.foldSmash(void"UPDATE t_exchange SET ", void", ", void" "))

        val where: Option[AppliedFragment] =
          NonEmptyList.fromList(which).map: oids =>
            sql"WHERE c_observation_id IN (${observation_id.nel(oids)})"(oids)

        (update, where)
          .mapN(_ |+| _)
          .traverse: af =>
            session.prepareR(af.fragment.command).use: ps =>
              ps.execute(af.argument)
          .void

      }

      override def clone(
        originalId: Observation.Id,
        newId: Observation.Id
      )(using Transaction[F]): F[Unit] =
        select(List(originalId)).map(_.get(originalId)).flatMap:
          case None         =>
            Concurrent[F].raiseError(new RuntimeException(s"Clone: original observation $originalId has no exchange config."))
          case Some(config) =>
            insert(config, List(newId)).flatMap: res =>
              res.toEither match
                case Left(Left(t))   => Concurrent[F].raiseError(t)
                case Left(Right(ps)) => Concurrent[F].raiseError(new RuntimeException(s"Clone: insert failed: ${ps.toList.mkString(", ")}"))
                case Right(a)        => a.pure

  private object Statements:

    val exchange_config: Codec[Config] =
      (
        keck_instrument.opt   *:
        subaru_instrument.opt *:
        time_span
      ).eimap {
         case (Some(keck), None, time)   => Config(keck.asLeft, time).asRight
         case (None, Some(subaru), time) => Config(subaru.asRight, time).asRight
         case _                          => Left("Expected one of c_keck_instrument or c_subaru_instrument")
      } { config => (config.keckInstrument, config.subaruInstrument, config.totalRequestTime) }

    def select[A <: NonEmptyList[Observation.Id]](a: A): Query[a.type, (Observation.Id, Config)] =
      sql"""
        SELECT
          c_observation_id,
          c_keck_instrument,
          c_subaru_instrument,
          c_total_request_time
        FROM
          t_exchange
        WHERE
           c_observation_id IN (${observation_id.nel(a)})
      """.query(observation_id *: exchange_config)

    val Insert: Command[(Observation.Id, ExchangeInput.Create)] =
      sql"""
        INSERT INTO t_exchange (
          c_observation_id,
          c_keck_instrument,
          c_subaru_instrument,
          c_total_request_time
        ) VALUES (
          $observation_id,
          ${keck_instrument.opt},
          ${subaru_instrument.opt},
          $time_span
        )
      """.command
        .contramap: (oid, config) =>
          (oid, config.keckInstrument, config.subaruInstrument, config.totalRequestTime)

    def delete[A <: NonEmptyList[Observation.Id]](a: A): Command[a.type] =
      sql"""
        DELETE FROM t_exchange
        WHERE c_observation_id IN (${observation_id.nel(a)})
      """.command