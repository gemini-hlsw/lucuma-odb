// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.model.Observation
import lucuma.odb.sequence.visitor.Config
import skunk.Transaction
import grackle.Result
import lucuma.odb.graphql.input.VisitorInput
import skunk.Command
import skunk.Query
import skunk.syntax.all.*
import cats.data.NonEmptyList
import lucuma.odb.util.Codecs.*
import cats.implicits.*
import lucuma.odb.service.Services.Syntax.session
import cats.effect.Concurrent
import skunk.data.Completion
import skunk.AppliedFragment

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
                SET.guideStarMinSep.foldMap(sql"guideStarMinSep = $angle_µas")
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
                            
  private object Statements:

    def select[A <: NonEmptyList[Observation.Id]](a: A): Query[a.type, (Observation.Id, Config)] =      
      sql"""
        SELECT 
          c_observation_id,      
          c_observing_mode_type, 
          c_central_wavelength,  
          c_guide_star_min_sep
        FROM
          t_visitor
        WHERE
           c_observation_id IN (${observation_id.nel(a)})
      """.query(observation_id *: visitor_observing_mode_type *: wavelength_pm *: angle_µas)
        .map:
          case (oid, mode, wavelength, sep) => (oid, Config(mode, wavelength, sep))

    val Insert: Command[(Observation.Id, VisitorInput.Create)] =
      sql"""
        INSERT INTO t_visitor (
          c_observation_id,      
          c_observing_mode_type, 
          c_central_wavelength,  
          c_guide_star_min_sep
        ) VALUES (
          $observation_id,
          $visitor_observing_mode_type,
          $wavelength_pm,
          $angle_µas
        )
      """.command
        .contramap:
          case (oid, VisitorInput.Create(mode, wavelength, angle)) => (oid, mode, wavelength, angle)

    def delete[A <: NonEmptyList[Observation.Id]](a: A): Command[a.type] =      
      sql"""
        DELETE FROM t_visitor
        WHERE c_observation_id IN (${observation_id.nel(a)})
      """.command
