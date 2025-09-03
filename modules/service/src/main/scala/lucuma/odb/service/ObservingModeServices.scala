// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.traverse.*
import grackle.Result
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.odb.graphql.input.ObservingModeInput
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.service.Services.SuperUserAccess
import skunk.Transaction

import Services.Syntax.*

sealed trait ObservingModeServices[F[_]] {

  def selectObservingMode(
    which: List[(Observation.Id, ObservingModeType)]
  )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, ObservingMode]]

  def createFunction(
    input: ObservingModeInput.Create
  )(using Transaction[F], SuperUserAccess): Result[List[Observation.Id] => F[Unit]]

  def deleteFunction(
    mode: ObservingModeType
  )(using Transaction[F], SuperUserAccess): List[Observation.Id] => F[Unit]

  def updateFunction(
    input: ObservingModeInput.Edit
  )(using Transaction[F], SuperUserAccess): Result[List[Observation.Id] => F[Unit]]

  def createViaUpdateFunction(
    input: ObservingModeInput.Edit
  )(using Transaction[F], SuperUserAccess): Result[List[Observation.Id] => F[Unit]]

  def cloneFunction(
    mode: ObservingModeType
  )(using Transaction[F], SuperUserAccess): (Observation.Id, Observation.Id) => F[Unit]

}

object ObservingModeServices {

  def instantiate[F[_]: {MonadCancelThrow, Services}]: ObservingModeServices[F] =
    new ObservingModeServices[F] {

      override def selectObservingMode(
        which: List[(Observation.Id, ObservingModeType)]
      )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, ObservingMode]] =
        import ObservingModeType.*

        which.groupMap(_._2)(_._1).toList.traverse {
          case (Flamingos2LongSlit, oids) =>
            flamingos2LongSlitService
              .select(oids)
              .map(_.widen[ObservingMode])

          case (GmosNorthLongSlit, oids) =>
            gmosLongSlitService
              .selectNorth(oids)
              .map(_.widen[ObservingMode])

          case (GmosNorthImaging, oids) =>
            gmosImagingService
              .selectNorth(oids)
              .map(_.widen[ObservingMode])

          case (GmosSouthLongSlit, oids) =>
            gmosLongSlitService
              .selectSouth(oids)
              .map(_.widen[ObservingMode])

          case (GmosSouthImaging, oids) =>
            gmosImagingService
              .selectSouth(oids)
              .map(_.widen[ObservingMode])

        }.map(_.fold(Map.empty[Observation.Id, ObservingMode])(_ ++ _))

      override def createFunction(
        input: ObservingModeInput.Create
      )(using Transaction[F], SuperUserAccess): Result[List[Observation.Id] => F[Unit]] =
        List(
          input.gmosNorthLongSlit.map(gmosLongSlitService.insertNorth),
          input.gmosSouthLongSlit.map(gmosLongSlitService.insertSouth),
          input.flamingos2LongSlit.map(flamingos2LongSlitService.insert),
          input.gmosNorthImaging.map(gmosImagingService.insertNorth),
          input.gmosSouthImaging.map(gmosImagingService.insertSouth)
        ).flattenOption match
          case List(f) => Result(f)
          case Nil     => Result.failure("No observing mode creation parameters were provided.")
          case _       => Result.failure("Only one observing mode's creation parameters may be provided.")

      override def deleteFunction(
        mode: ObservingModeType
      )(using Transaction[F], SuperUserAccess): List[Observation.Id] => F[Unit] =
        mode match
          case ObservingModeType.Flamingos2LongSlit => flamingos2LongSlitService.delete
          case ObservingModeType.GmosNorthLongSlit  => gmosLongSlitService.deleteNorth
          case ObservingModeType.GmosNorthImaging   => gmosImagingService.deleteNorth
          case ObservingModeType.GmosSouthLongSlit  => gmosLongSlitService.deleteSouth
          case ObservingModeType.GmosSouthImaging   => gmosImagingService.deleteSouth

      override def updateFunction(
        input: ObservingModeInput.Edit
      )(using Transaction[F], SuperUserAccess): Result[List[Observation.Id] => F[Unit]] =
        List(
          input.flamingos2LongSlit.map(flamingos2LongSlitService.update),
          input.gmosNorthLongSlit.map(gmosLongSlitService.updateNorth),
          input.gmosSouthLongSlit.map(gmosLongSlitService.updateSouth),
          input.gmosNorthImaging.map(gmosImagingService.updateNorth),
          input.gmosSouthImaging.map(gmosImagingService.updateSouth)
        ).flattenOption match
          case List(f) => Result(f)
          case Nil     => Result.failure("No observing mode edit parameters were provided.")
          case _       => Result.failure("Only one observing mode's edit parameters may be provided.")

      override def createViaUpdateFunction(
        input: ObservingModeInput.Edit
      )(using Transaction[F], SuperUserAccess): Result[List[Observation.Id] => F[Unit]] =
        List(
          input.flamingos2LongSlit.map(m => m.toCreate.map(flamingos2LongSlitService.insert)),
          input.gmosNorthLongSlit.map(m => m.toCreate.map(gmosLongSlitService.insertNorth)),
          input.gmosSouthLongSlit.map(m => m.toCreate.map(gmosLongSlitService.insertSouth)),
          input.gmosNorthImaging.map(m => m.toCreate.map(gmosImagingService.insertNorth)),
          input.gmosSouthImaging.map(m => m.toCreate.map(gmosImagingService.insertSouth))
        ).flattenOption match
          case List(f) => f
          case Nil     => Result.failure("No observing mode edit parameters were provided.")
          case _       => Result.failure("Only one observing mode's edit parameters may be provided.")

      override def cloneFunction(
        mode: ObservingModeType
      )(using Transaction[F], SuperUserAccess): (Observation.Id, Observation.Id) => F[Unit] =
        mode match
          case ObservingModeType.Flamingos2LongSlit => flamingos2LongSlitService.clone
          case ObservingModeType.GmosNorthLongSlit  => gmosLongSlitService.cloneNorth
          case ObservingModeType.GmosNorthImaging   => gmosImagingService.cloneNorth
          case ObservingModeType.GmosSouthLongSlit  => gmosLongSlitService.cloneSouth
          case ObservingModeType.GmosSouthImaging   => gmosImagingService.cloneSouth

    }

}
