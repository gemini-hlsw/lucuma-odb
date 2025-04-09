// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.traverse.*
import grackle.Result
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.SourceProfile
import lucuma.odb.graphql.input.ObservingModeInput
import lucuma.odb.sequence.ObservingMode
import skunk.Transaction

import Services.Syntax.*

sealed trait ObservingModeServices[F[_]] {

  def selectObservingMode(
    which: List[(Observation.Id, ObservingModeType)]
  )(using Transaction[F]): F[Map[Observation.Id, SourceProfile => ObservingMode]]

  def createFunction(
    input: ObservingModeInput.Create
  ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]]

  def deleteFunction(
    mode: ObservingModeType
  ): (List[Observation.Id], Transaction[F]) => F[Unit]

  def updateFunction(
    input: ObservingModeInput.Edit
  ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]]

  def createViaUpdateFunction(
    input: ObservingModeInput.Edit
  ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]]

  def cloneFunction(
    mode: ObservingModeType
  ): (Observation.Id, Observation.Id) => F[Unit]

}

object ObservingModeServices {

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): ObservingModeServices[F] =
    new ObservingModeServices[F] {

      override def selectObservingMode(
        which: List[(Observation.Id, ObservingModeType)]
      )(using Transaction[F]): F[Map[Observation.Id, SourceProfile => ObservingMode]] = {
        import ObservingModeType.*

        which.groupMap(_._2)(_._1).toList.traverse {
          case (GmosNorthLongSlit, oids) =>
            gmosLongSlitService
              .selectNorth(oids)
              .map(_.view.mapValues(_.widen[ObservingMode]).toMap)

          case (GmosSouthLongSlit, oids) =>
            gmosLongSlitService
              .selectSouth(oids)
              .map(_.view.mapValues(_.widen[ObservingMode]).toMap)

          case (Flamingos2LongSlit, _) => ???
        }.map(_.fold(Map.empty[Observation.Id, SourceProfile => ObservingMode])(_ ++ _))
      }

      override def createFunction(
        input: ObservingModeInput.Create
      ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]] =
        List(
          input.gmosNorthLongSlit.map(gmosLongSlitService.insertNorth),
          input.gmosSouthLongSlit.map(gmosLongSlitService.insertSouth)
        ).flattenOption match {
          case List(f) => Result(f)
          case Nil     => Result.failure("No observing mode creation parameters were provided.")
          case _       => Result.failure("Only one observing mode's creation parameters may be provided.")
        }

      override def deleteFunction(
        mode: ObservingModeType
      ): (List[Observation.Id], Transaction[F]) => F[Unit] =
        mode match {
          case ObservingModeType.GmosNorthLongSlit  => gmosLongSlitService.deleteNorth
          case ObservingModeType.GmosSouthLongSlit  => gmosLongSlitService.deleteSouth
          case ObservingModeType.Flamingos2LongSlit => ???
        }

      override def updateFunction(
        input: ObservingModeInput.Edit
      ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]] =
        List(
          input.gmosNorthLongSlit.map(gmosLongSlitService.updateNorth),
          input.gmosSouthLongSlit.map(gmosLongSlitService.updateSouth)
        ).flattenOption match {
          case List(f) => Result(f)
          case Nil     => Result.failure("No observing mode edit parameters were provided.")
          case _       => Result.failure("Only one observing mode's edit parameters may be provided.")
        }

      override def createViaUpdateFunction(
        input: ObservingModeInput.Edit
      ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]] =
        List(
          input.gmosNorthLongSlit.map(m => m.toCreate.map(gmosLongSlitService.insertNorth)),
          input.gmosSouthLongSlit.map(m => m.toCreate.map(gmosLongSlitService.insertSouth))
        ).flattenOption match {
          case List(f) => f
          case Nil     => Result.failure("No observing mode edit parameters were provided.")
          case _       => Result.failure("Only one observing mode's edit parameters may be provided.")
        }

      override def cloneFunction(
        mode: ObservingModeType
      ): (Observation.Id, Observation.Id) => F[Unit] =
        mode match {
          case ObservingModeType.GmosNorthLongSlit  => gmosLongSlitService.cloneNorth
          case ObservingModeType.GmosSouthLongSlit  => gmosLongSlitService.cloneSouth
          case ObservingModeType.Flamingos2LongSlit => ???
        }

    }

}
