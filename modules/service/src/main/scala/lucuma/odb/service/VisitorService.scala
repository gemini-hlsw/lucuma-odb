// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.implicits.*
import grackle.Result
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.model.Observation
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.VisitorInput
import lucuma.odb.sequence.visitor.Config
import lucuma.odb.service.Services.Syntax.session
import lucuma.odb.util.Codecs.*
import skunk.AppliedFragment
import skunk.Command
import skunk.Query
import skunk.Transaction
import skunk.data.Completion
import skunk.syntax.all.*

trait VisitorService[F[_]]:

  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config]]

  def insert(
    input: VisitorInput.Create,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def update(
    SET: VisitorInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def clone(
    originalId: Observation.Id,
    newId: Observation.Id
  )(using Transaction[F]): F[Unit]

object VisitorService:

  def instantiate[F[_]: Concurrent](using Services[F]): VisitorService[F] =
    new VisitorService[F]:

      override def select(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        NonEmptyList.fromList(which) match
          case None      => Map.empty.pure
          case Some(nel) =>
            session.prepareR(Statements.select(nel)).use: pq =>
              pq.stream(nel, 1024).compile.toList.map(_.toMap)

      override def insert(
        input: VisitorInput.Create,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        validateRequiredFields(input.mode, input.name.isDefined, input.totalRequestTime.isDefined) match
          case Left(err) => err.asFailure.pure
          case Right(_)  =>
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
        SET: VisitorInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] = {

        val update: Option[AppliedFragment] =
          NonEmptyList
            .fromList:
              List(
                SET.centralWavelength.foldMap(sql"centralWavelength = $wavelength_pm"),
                SET.mode.foldMap(sql"mode = $visitor_observing_mode_type"),
                SET.scienceFov.foldMap(sql"c_science_fov = $angle_µas"),
                SET.name.foldMap(sql"c_name = $text_nonempty"),
                SET.totalRequestTime.foldMap(sql"c_total_request_time = $time_span")
              )
            .map(_.foldSmash(void"UPDATE t_visitor SET ", void",", void" "))

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
            Concurrent[F].raiseError(new RuntimeException(s"Clone: original observation $originalId has no visitor config."))
          case Some(config) =>
            insert(config, List(newId)).flatMap: res =>
              res.toEither match
                case Left(Left(t))   => Concurrent[F].raiseError(t)
                case Left(Right(ps)) => Concurrent[F].raiseError(new RuntimeException(s"Clone: insert failed: ${ps.toList.mkString(", ")}"))
                case Right(a)        => a.pure

  // Visitor require both a name and a total request time.
  private def validateRequiredFields(
    mode:             VisitorObservingModeType,
    nameDefined:      Boolean,
    totalTimeDefined: Boolean
  ): Either[OdbError, Unit] =
    mode match
      case VisitorObservingModeType.VisitorNorth | VisitorObservingModeType.VisitorSouth
        if !nameDefined || !totalTimeDefined =>
        Left(OdbError.InvalidArgument(
          s"Visitor mode $mode requires both `name` and `totalRequestTime` to be provided.".some
        ))
      case _ => Right(())

  private object Statements:

    def select[A <: NonEmptyList[Observation.Id]](a: A): Query[a.type, (Observation.Id, Config)] =
      sql"""
        SELECT
          c_observation_id,
          c_observing_mode_type,
          c_central_wavelength,
          c_science_fov,
          c_name,
          c_total_request_time
        FROM
          t_visitor
        WHERE
           c_observation_id IN (${observation_id.nel(a)})
      """.query(observation_id *: visitor_observing_mode_type *: wavelength_pm *: angle_µas *: text_nonempty.opt *: time_span.opt)
        .map:
          case (oid, mode, wavelength, sep, name, trt) => (oid, Config(mode, wavelength, sep, name, trt))

    val Insert: Command[(Observation.Id, VisitorInput.Create)] =
      sql"""
        INSERT INTO t_visitor (
          c_observation_id,
          c_observing_mode_type,
          c_central_wavelength,
          c_science_fov,
          c_name,
          c_total_request_time
        ) VALUES (
          $observation_id,
          $visitor_observing_mode_type,
          $wavelength_pm,
          $angle_µas,
          ${text_nonempty.opt},
          ${time_span.opt}
        )
      """.command
        .contramap:
          case (oid, VisitorInput.Create(mode, wavelength, angle, name, trt)) =>
            (oid, mode, wavelength, angle, name, trt)

    def delete[A <: NonEmptyList[Observation.Id]](a: A): Command[a.type] =
      sql"""
        DELETE FROM t_visitor
        WHERE c_observation_id IN (${observation_id.nel(a)})
      """.command
