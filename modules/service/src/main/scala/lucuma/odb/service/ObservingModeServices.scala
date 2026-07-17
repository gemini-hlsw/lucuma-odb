// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.option.*
import cats.syntax.applicative.*
import cats.syntax.traverse.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.VisitorObservingModeType
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

          case (e: ExchangeObservingModeType, oids) =>
            exchangeService
              .select(oids)
              .map(_.widen[ObservingMode])

          case (Flamingos2LongSlit, oids) =>
            flamingos2LongSlitService
              .select(oids)
              .map(_.widen[ObservingMode])

          case (Flamingos2Imaging, oids) =>
            flamingos2ImagingService
              .select(oids)
              .map(_.widen[ObservingMode])

          case (GhostIfu, oids) =>
            ghostIfuService
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

          case (GmosNorthMos, oids) =>
            Map.empty.pure[F] // N.B., GMOS North MOS is not yet supported.

          case (GmosSouthLongSlit, oids) =>
            gmosLongSlitService
              .selectSouth(oids)
              .map(_.widen[ObservingMode])

          case (GmosSouthImaging, oids) =>
            gmosImagingService
              .selectSouth(oids)
              .map(_.widen[ObservingMode])

          case (GnirsImaging, oids) =>
            gnirsImagingService
              .select(oids)
              .map(_.widen[ObservingMode])

          case (GmosSouthMos, oids) =>
            Map.empty.pure[F] // N.B., GMOS South MOS is not yet supported.

          case (GnirsLongSlit | GnirsIfu, oids) =>
            gnirsSpectroscopyService
              .select(oids)
              .map(_.widen[ObservingMode])

          case (Igrins2LongSlit, oids) =>
            igrins2LongSlitService
              .select(oids)
              .map(_.widen[ObservingMode])

          case (v: VisitorObservingModeType, oids) =>
            visitorService
              .select(oids)
              .map(_.widen[ObservingMode])

        }.map(_.fold(Map.empty[Observation.Id, ObservingMode])(_ ++ _))

      //------------------------------------------------------------------------
      // N.B. Whenever a new mode is added to the database, recall that the
      // database migration requires registering it in `t_observing_mode_registry`
      // via:
      //
      //     SELECT register_observing_mode(mode_type, table_name);
      //
      // For example:
      //
      //     SELECT register_observing_mode('flamingos_2_long_slit', 't_flamingos_2_long_slit');
      //
      // This enables a trigger that checks whether the observation's observing
      // mode is consistent with an entry in the corresponding observing mode
      // table.
      //------------------------------------------------------------------------

      override def create(
        input: ObservingModeInput.Create,
        etm:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F], SuperUserAccess): F[Result[Unit]] =
        List(
          input.exchange.map(m =>           exchangeService.insert(m, which)),
          input.flamingos2Imaging.map(m =>  flamingos2ImagingService.insert(m, etm, which)),
          input.flamingos2LongSlit.map(m => flamingos2LongSlitService.insert(m, etm, which)),
          input.ghostIfu.map(m =>           ghostIfuService.insert(m, etm, which)),
          input.gmosNorthImaging.map(m =>   gmosImagingService.insertNorth(m, etm, which)),
          input.gmosNorthLongSlit.map(m =>  gmosLongSlitService.insertNorth(m, etm, which)),
          input.gmosSouthImaging.map(m =>   gmosImagingService.insertSouth(m, etm, which)),
          input.gmosSouthLongSlit.map(m =>  gmosLongSlitService.insertSouth(m, etm, which)),
          input.igrins2LongSlit.map(m =>    igrins2LongSlitService.insert(m, etm, which)),
          input.gnirsImaging.map(m =>       gnirsImagingService.insert(m, etm, which)),
          input.gnirsSpectroscopy.map(m =>  gnirsSpectroscopyService.insert(m, etm, which)),
          input.visitor.map(m => visitorService.insert(m, which)),
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
            case _: ExchangeObservingModeType         => exchangeService.delete(which)
            case ObservingModeType.Flamingos2LongSlit => flamingos2LongSlitService.delete(which)
            case ObservingModeType.Flamingos2Imaging  => flamingos2ImagingService.delete(which)
            case ObservingModeType.GhostIfu           => ghostIfuService.delete(which)
            case ObservingModeType.GmosNorthImaging   => gmosImagingService.deleteNorth(which)
            case ObservingModeType.GmosNorthLongSlit  => gmosLongSlitService.deleteNorth(which)
            case ObservingModeType.GmosNorthMos       => MonadCancelThrow[F].raiseError(new RuntimeException("GMOS North MOS is not yet supported."))
            case ObservingModeType.GmosSouthImaging   => gmosImagingService.deleteSouth(which)
            case ObservingModeType.GmosSouthLongSlit  => gmosLongSlitService.deleteSouth(which)
            case ObservingModeType.GmosSouthMos       => MonadCancelThrow[F].raiseError(new RuntimeException("GMOS South MOS is not yet supported."))
            case ObservingModeType.GnirsImaging       => gnirsImagingService.delete(which)
            case ObservingModeType.GnirsLongSlit | ObservingModeType.GnirsIfu => gnirsSpectroscopyService.delete(which)
            case ObservingModeType.Igrins2LongSlit    => igrins2LongSlitService.delete(which)
            case _: VisitorObservingModeType          => visitorService.delete(which)

        deleteObservingMode *> deleteExposureTimeModes

      def update(
        input: ObservingModeInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F], SuperUserAccess): F[Result[Unit]] =
        List(
          input.exchange.map(m => exchangeService.update(m, which).map(_.success)),
          input.flamingos2Imaging.map(m => flamingos2ImagingService.update(m, which)),
          input.flamingos2LongSlit.map(m => flamingos2LongSlitService.update(m, which).map(_.success)),
          input.ghostIfu.map(m => ghostIfuService.update(m, which)),
          input.gmosNorthImaging.map(m => gmosImagingService.updateNorth(m, which)),
          input.gmosNorthLongSlit.map(m => gmosLongSlitService.updateNorth(m, which).map(_.success)),
          input.gmosSouthImaging.map(m => gmosImagingService.updateSouth(m, which)),
          input.gmosSouthLongSlit.map(m => gmosLongSlitService.updateSouth(m, which).map(_.success)),
          input.igrins2LongSlit.map(m => igrins2LongSlitService.update(m, which).map(_.success)),
          input.gnirsImaging.map(m => gnirsImagingService.update(m, which)),
          input.gnirsSpectroscopy.map(m => gnirsSpectroscopyService.update(m, which)),
          input.visitor.map(m => visitorService.update(m, which).map(_.success))
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
            case _: ExchangeObservingModeType         => exchangeService.clone(origOid, newOid)
            case ObservingModeType.Flamingos2LongSlit => flamingos2LongSlitService.clone(origOid, newOid)
            case ObservingModeType.Flamingos2Imaging  => flamingos2ImagingService.clone(origOid, newOid, etms)
            case ObservingModeType.GhostIfu           => ghostIfuService.clone(origOid, newOid, etms)
            case ObservingModeType.GmosNorthLongSlit  => gmosLongSlitService.cloneNorth(origOid, newOid)
            case ObservingModeType.GmosNorthImaging   => gmosImagingService.cloneNorth(origOid, newOid, etms)
            case ObservingModeType.GmosNorthMos       => MonadCancelThrow[F].raiseError(new RuntimeException("GMOS North MOS is not yet supported."))
            case ObservingModeType.GmosSouthLongSlit  => gmosLongSlitService.cloneSouth(origOid, newOid)
            case ObservingModeType.GmosSouthImaging   => gmosImagingService.cloneSouth(origOid, newOid, etms)
            case ObservingModeType.GmosSouthMos       => MonadCancelThrow[F].raiseError(new RuntimeException("GMOS South MOS is not yet supported."))
            case ObservingModeType.GnirsImaging       => gnirsImagingService.clone(origOid, newOid, etms)
            case ObservingModeType.GnirsLongSlit | ObservingModeType.GnirsIfu => gnirsSpectroscopyService.clone(origOid, newOid)
            case ObservingModeType.Igrins2LongSlit    => igrins2LongSlitService.clone(origOid, newOid)
            case _: VisitorObservingModeType          => visitorService.clone(origOid, newOid)

        exposureTimeModeService
          .clone(origOid, newOid)
          .flatMap(cloneObservingMode)
