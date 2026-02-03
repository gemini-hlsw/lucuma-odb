// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.syntax.applicative.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.ObservingModeInput
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.service.Services.SuperUserAccess
import skunk.Transaction

import Services.Syntax.*

// N.B., observing modes come with an acquisition exposure time mode and one or
// more science exposure time modes.  This class directly handles ETM updates
// for delete and clone, but create, update, and createViaUpdate implementations
// do their own ETM management.  This is because the rules will differ depending
// on the observing mode.

sealed trait ObservingModeServices[F[_]]:

  def selectObservingMode(
    which: List[(Observation.Id, ObservingModeType)]
  )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, ObservingMode]]

  def create(
    input: ObservingModeInput.Create,
    etm:   Option[ExposureTimeMode],
    which: List[Observation.Id]
  )(using Transaction[F], SuperUserAccess): F[Result[Unit]]

  def delete(
    mode:  ObservingModeType,
    which: List[Observation.Id]
  )(using Transaction[F], SuperUserAccess): F[Unit]

  def update(
    input: ObservingModeInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F], SuperUserAccess): F[Result[Unit]]

  def createViaUpdate(
    input: ObservingModeInput.Edit,
    etm:   Option[ExposureTimeMode],
    which: List[Observation.Id]
  )(using Transaction[F], SuperUserAccess): F[Result[Unit]]

  def clone(
    mode:    ObservingModeType,
    origOid: Observation.Id,
    newOid:  Observation.Id
  )(using Transaction[F], SuperUserAccess): F[Unit]

object ObservingModeServices:

  def instantiate[F[_]: {MonadCancelThrow, Services}]: ObservingModeServices[F] =
    new ObservingModeServices[F]:

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

          case (Igrins2LongSlit, oids) =>
            Map.empty.pure[F]

        }.map(_.fold(Map.empty[Observation.Id, ObservingMode])(_ ++ _))

      override def create(
        input: ObservingModeInput.Create,
        etm:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F], SuperUserAccess): F[Result[Unit]] =
        List(
          input.flamingos2LongSlit.map(m => flamingos2LongSlitService.insert(m, etm, which)),
          input.gmosNorthImaging.map(m =>   gmosImagingService.insertNorth(m, etm, which)),
          input.gmosNorthLongSlit.map(m =>  gmosLongSlitService.insertNorth(m, etm, which)),
          input.gmosSouthImaging.map(m =>   gmosImagingService.insertSouth(m, etm, which)),
          input.gmosSouthLongSlit.map(m =>  gmosLongSlitService.insertSouth(m, etm, which))
        ).flattenOption match
          case List(r) => r
          case Nil     => OdbError.InvalidArgument("No observing mode creation parameters were provided.".some).asFailureF
          case _       => OdbError.InvalidArgument("Only one observing mode's creation parameters may be provided.".some).asFailureF

      override def delete(
        mode:  ObservingModeType,
        which: List[Observation.Id]
      )(using Transaction[F], SuperUserAccess): F[Unit] =

        val deleteExposureTimeModes: F[Unit] =
          services.exposureTimeModeService.deleteMany(which, ExposureTimeModeRole.Acquisition, ExposureTimeModeRole.Science)

        val deleteObservingMode: F[Unit] =
          mode match
            case ObservingModeType.Flamingos2LongSlit => flamingos2LongSlitService.delete(which)
            case ObservingModeType.GmosNorthImaging   => gmosImagingService.deleteNorth(which)
            case ObservingModeType.GmosNorthLongSlit  => gmosLongSlitService.deleteNorth(which)
            case ObservingModeType.GmosSouthImaging   => gmosImagingService.deleteSouth(which)
            case ObservingModeType.GmosSouthLongSlit  => gmosLongSlitService.deleteSouth(which)
            // FIXME
            case ObservingModeType.Igrins2LongSlit    => ().pure[F]

        deleteExposureTimeModes *> deleteObservingMode

      def update(
        input: ObservingModeInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F], SuperUserAccess): F[Result[Unit]] =
        List(
          input.flamingos2LongSlit.map(m => flamingos2LongSlitService.update(m, which).map(_.success)),
          input.gmosNorthImaging.map(m => gmosImagingService.updateNorth(m, which)),
          input.gmosNorthLongSlit.map(m => gmosLongSlitService.updateNorth(m, which).map(_.success)),
          input.gmosSouthImaging.map(m => gmosImagingService.updateSouth(m, which)),
          input.gmosSouthLongSlit.map(m => gmosLongSlitService.updateSouth(m, which).map(_.success))
        ).flattenOption match
          case List(r) => r
          case Nil     => OdbError.InvalidArgument("No observing mode edit parameters were provided.".some).asFailureF
          case _       => OdbError.InvalidArgument("Only one observing mode's edit parameters may be provided.".some).asFailureF

      override def createViaUpdate(
        input: ObservingModeInput.Edit,
        etm:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F], SuperUserAccess): F[Result[Unit]] =
        input.toCreate.flatTraverse(c => create(c, etm, which))

      override def clone(
        mode:    ObservingModeType,
        origOid: Observation.Id,
        newOid:  Observation.Id
      )(using Transaction[F], SuperUserAccess): F[Unit] =

        def cloneObservingMode(etms: List[(ExposureTimeModeId, ExposureTimeModeId)]): F[Unit] =
          mode match
            case ObservingModeType.Flamingos2LongSlit => flamingos2LongSlitService.clone(origOid, newOid)
            case ObservingModeType.GmosNorthLongSlit  => gmosLongSlitService.cloneNorth(origOid, newOid)
            case ObservingModeType.GmosNorthImaging   => gmosImagingService.cloneNorth(origOid, newOid, etms)
            case ObservingModeType.GmosSouthLongSlit  => gmosLongSlitService.cloneSouth(origOid, newOid)
            case ObservingModeType.GmosSouthImaging   => gmosImagingService.cloneSouth(origOid, newOid, etms)
            // FIXME
            case ObservingModeType.Igrins2LongSlit    => ().pure[F]

        exposureTimeModeService
          .clone(origOid, newOid)
          .flatMap(cloneObservingMode)
