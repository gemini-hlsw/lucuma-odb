// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import grackle.Result
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.ObservingModeInput
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.service.Services.SuperUserAccess
import skunk.Transaction

import Services.Syntax.*

sealed trait ObservingModeServices[F[_]] {

  def selectObservingMode(
    which: List[(Observation.Id, ObservingModeType)]
  )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, ObservingMode]]

  def create(
    input: ObservingModeInput.Create,
    etm:   Option[ExposureTimeMode],
    which: List[Observation.Id]
  )(using Transaction[F], SuperUserAccess): F[Result[Unit]]

  def deleteFunction(
    mode: ObservingModeType
  )(using Transaction[F], SuperUserAccess): List[Observation.Id] => F[Unit]

  def updateFunction(
    input: ObservingModeInput.Edit
  )(using Transaction[F], SuperUserAccess): Result[List[Observation.Id] => F[Unit]]

  def createViaUpdate(
    input: ObservingModeInput.Edit,
    etm:   Option[ExposureTimeMode],
    which: List[Observation.Id]
  )(using Transaction[F], SuperUserAccess): F[Result[Unit]]

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

      override def create(
        input: ObservingModeInput.Create,
        etm:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F], SuperUserAccess): F[Result[Unit]] =
        List(
          input.flamingos2LongSlit.map(m => flamingos2LongSlitService.insert(m, etm, which)),
          input.gmosNorthImaging.map(m => gmosImagingService.insertNorth(m, etm, which)),
          input.gmosNorthLongSlit.map(m => gmosLongSlitService.insertNorth(m, etm, which)),
          input.gmosSouthImaging.map(m => gmosImagingService.insertSouth(m, etm, which)),
          input.gmosSouthLongSlit.map(m => gmosLongSlitService.insertSouth(m, etm, which))
        ).flattenOption match
          case List(r) => r
          case Nil     => OdbError.InvalidArgument("No observing mode creation parameters were provided.".some).asFailureF
          case _       => OdbError.InvalidArgument("Only one observing mode's creation parameters may be provided.".some).asFailureF

      override def deleteFunction(
        mode: ObservingModeType
      )(using Transaction[F], SuperUserAccess): List[Observation.Id] => F[Unit] =
        mode match
          case ObservingModeType.Flamingos2LongSlit => flamingos2LongSlitService.delete
          case ObservingModeType.GmosNorthImaging   => gmosImagingService.deleteNorth
          case ObservingModeType.GmosNorthLongSlit  => gmosLongSlitService.deleteNorth
          case ObservingModeType.GmosSouthImaging   => gmosImagingService.deleteSouth
          case ObservingModeType.GmosSouthLongSlit  => gmosLongSlitService.deleteSouth

      override def updateFunction(
        input: ObservingModeInput.Edit
      )(using Transaction[F], SuperUserAccess): Result[List[Observation.Id] => F[Unit]] =
        List(
          input.flamingos2LongSlit.map(flamingos2LongSlitService.update),
          input.gmosNorthImaging.map(gmosImagingService.updateNorth),
          input.gmosNorthLongSlit.map(gmosLongSlitService.updateNorth),
          input.gmosSouthImaging.map(gmosImagingService.updateSouth),
          input.gmosSouthLongSlit.map(gmosLongSlitService.updateSouth)
        ).flattenOption match
          case List(f) => Result(f)
          case Nil     => Result.failure("No observing mode edit parameters were provided.")
          case _       => Result.failure("Only one observing mode's edit parameters may be provided.")

      override def createViaUpdate(
        input: ObservingModeInput.Edit,
        etm:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F], SuperUserAccess): F[Result[Unit]] =
        List(
          input.flamingos2LongSlit.map(m => m.toCreate.flatTraverse(c => flamingos2LongSlitService.insert(c, etm, which))),
          input.gmosNorthImaging.map(m => m.toCreate.flatTraverse(c => gmosImagingService.insertNorth(c, etm, which))),
          input.gmosNorthLongSlit.map(m => m.toCreate.flatTraverse(c => gmosLongSlitService.insertNorth(c, etm, which))),
          input.gmosSouthLongSlit.map(m => m.toCreate.flatTraverse(c => gmosLongSlitService.insertSouth(c, etm, which))),
          input.gmosSouthImaging.map(m => m.toCreate.flatTraverse(c => gmosImagingService.insertSouth(c, etm, which)))
        ).flattenOption match
          case List(r) => r
          case Nil     => OdbError.InvalidArgument("No observing mode creation parameters were provided.".some).asFailureF
          case _       => OdbError.InvalidArgument("Only one observing mode's creation parameters may be provided.".some).asFailureF

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
