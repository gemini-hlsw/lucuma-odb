// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order.catsKernelOrderingForOrder
import cats.data.Nested
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.odb.Config
import lucuma.odb.graphql.input.EditAsterismsPatchInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ItcInput
import lucuma.odb.service.CalibrationConfigSubset.*
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import lucuma.refined.*
import org.http4s.client.Client
import skunk.AppliedFragment
import skunk.Command
import skunk.Query
import skunk.Transaction
import skunk.codec.numeric.int8
import skunk.codec.text.text
import skunk.syntax.all.*

import java.time.Instant

trait CalibrationsService[F[_]] {
  def setCalibrationRole(
    oid:  Observation.Id,
    role: Option[CalibrationRole]
  )(using Transaction[F], ServiceAccess): F[Unit]

  /**
    * Recalculates the calibrations for a program
    *
    * @param pid Program.Id
    * @param referenceInstant time used to calculate targets
    * @return list of added and removed calibration observations
    */
  def recalculateCalibrations(
    pid: Program.Id,
    referenceInstant: Instant
  )(using Transaction[F], ServiceAccess): F[(List[Observation.Id], List[Observation.Id])]

  /**
    * Returns the calibration targets for a given role adjusted to a reference instant
    */
  def calibrationTargets(roles: List[CalibrationRole], referenceInstant: Instant): F[List[(Target.Id, String, CalibrationRole, Coordinates)]]

  def recalculateCalibrationTarget(
    pid: Program.Id,
    oid: Observation.Id,
  )(using Transaction[F], ServiceAccess): F[Unit]
}

object CalibrationsService extends CalibrationObservations {
  val CalibrationsGroupName: NonEmptyString = "Calibrations".refined
  val CalibrationTypes = List(CalibrationRole.SpectroPhotometric, CalibrationRole.Twilight)

  case class ObsExtract[A](
    id:   Observation.Id,
    itc:  Option[ItcInput],
    band: Option[ScienceBand],
    role: Option[CalibrationRole],
    data: A
  ):
    def map[B](f: A => B): ObsExtract[B] =
      copy(data = f(data))

  case class CalObsProps(
    wavelengthAt: Option[Wavelength],
    band:         Option[ScienceBand]
  )

  private def targetCoordinates(when: Instant)(
    rows: List[(Target.Id, String, CalibrationRole, RightAscension, Declination, Epoch, Option[Long], Option[Long], Option[RadialVelocity], Option[Parallax])]
  ): List[(Target.Id, String, CalibrationRole, Coordinates)] =
    rows.map { case (tid, name, role, ra, dec, epoch, pmra, pmdec, rv, parallax) =>
      (tid,
        name,
        role,
        SiderealTracking(
          Coordinates(ra, dec),
          epoch,
          (pmra, pmdec).mapN{ case (r, d) =>
            ProperMotion(ProperMotion.μasyRA(r), ProperMotion.μasyDec(d))
          },
          rv,
          parallax
        ).at(when)
      )
    }.collect {
      case (tid, name, role, Some(st)) => (tid, name, role, st)
    }

  def instantiate[F[_]: MonadCancelThrow](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): CalibrationsService[F] =
    new CalibrationsService[F] {

      override def setCalibrationRole(
        oid:  Observation.Id,
        role: Option[CalibrationRole]
      )(using Transaction[F], ServiceAccess): F[Unit] =
        session.execute(Statements.SetCalibrationRole)(oid, role).void

      private def collectValid(
        requiresItcInputs: Boolean
      ): PartialFunction[(Observation.Id, Either[GeneratorParamsService.Error, GeneratorParams]), ObsExtract[ObservingMode]] =
        {
          case (oid, Right(GeneratorParams(itc, band, mode, calibRole, _, _))) if itc.isRight || !requiresItcInputs =>
            ObsExtract(oid, itc.toOption, band, calibRole, mode)
        }

      // Find all active observations in the program
      private def activeObservations(pid: Program.Id): F[Set[Observation.Id]] =
        session.execute(Statements.selectActiveObservations)(pid).map(_.toSet)

      /**
       * Requests configuration params for a program id and selection (either science or calibration)
       */
      private def allObservations(
        pid:       Program.Id,
        selection: ObservationSelection
      )(using Transaction[F]): F[List[ObsExtract[ObservingMode]]] =
        services
          .generatorParamsService
          .selectAll(pid, selection = selection)
          .map(_.toList.collect(collectValid(selection === ObservationSelection.Science)))

      private def allUnexecutedObservations(
        pid:       Program.Id,
        selection: ObservationSelection
      )(using Transaction[F]): F[List[ObsExtract[ObservingMode]]] =
        services
          .generatorParamsService
          .selectAllUnexecuted(pid, selection = selection)
          .map(_.toList.collect(collectValid(selection === ObservationSelection.Science)))

      private def toConfigForCalibration(all: List[ObsExtract[ObservingMode]]): List[ObsExtract[CalibrationConfigSubset]] =
        all.map(_.map(_.toConfigSubset))

      override def calibrationTargets(roles: List[CalibrationRole], referenceInstant: Instant)
        : F[List[(Target.Id, String, CalibrationRole, Coordinates)]] =
          session.execute(Statements.selectCalibrationTargets(roles))(roles)
            .map(targetCoordinates(referenceInstant))

      def recalculateCalibrations(pid: Program.Id, referenceInstant: Instant)(using Transaction[F], ServiceAccess): F[(List[Observation.Id], List[Observation.Id])] =
        val sharedService = SharedCalibrationsService.instantiate(emailConfig, httpClient)
        val perObsService = PerObsCalibrationsService.instantiate(emailConfig, httpClient)

        for
          // Read calibration targets (shared resource)
          calibTargets <- calibrationTargets(CalibrationTypes, referenceInstant)

          // List of the program's active observations
          active       <- activeObservations(pid)

          // Get all active science and calibration observations
          allSci       <- allObservations(pid, ObservationSelection.Science)
                            .map(_.filter(u => active.contains(u._1)))
          allCalibs    <- allUnexecutedObservations(pid, ObservationSelection.Calibration)
                            .map(_.filter(u => active.contains(u.id)))

          // Delegate to shared calibrations service (handles GMOS)
          sharedResult <- sharedService.generateSharedCalibrations(pid, allSci, allCalibs, calibTargets, referenceInstant)
          (addedShared, removedShared) = sharedResult

          // Delegate to per-observation calibrations service (handles F2 tellurics)
          f2ScienceObs  = toConfigForCalibration(allSci.filter(_.data.toConfigSubset.isInstanceOf[Flamingos2Configs]))
          addedPerObs  <- perObsService.generatePerObsCalibrations(pid, f2ScienceObs)

          // Cleanup
          _            <- targetService.deleteOrphanCalibrationTargets(pid)
        yield (addedShared ::: addedPerObs, removedShared)

      // Recalcula the target of a calibration observation
      def recalculateCalibrationTarget(
        pid: Program.Id,
        oid: Observation.Id,
      )(using Transaction[F], ServiceAccess): F[Unit] = {
        for {
          o    <- session.execute(Statements.selectCalibrationTimeAndConf)(oid).map(_.headOption)
          // Find the original target
          otgs <- o.map(_._1).map { oid =>
                    Services.asSuperUser:
                      asterismService.getAsterism(pid, oid).map(_.map(_._1))
                  }.getOrElse(List.empty.pure[F])
          // Select a new target
          tgts <- o match {
                  case Some(oid, cr, Some(ot), Some(ObservingModeType.GmosNorthLongSlit)) =>
                    session
                      .execute(Statements.selectCalibrationTargets(CalibrationTypes))(CalibrationTypes)
                      .map(targetCoordinates(ot.toInstant).map(CalibrationIdealTargets(Site.GN, ot.toInstant, _)).map(_.bestTarget(cr)))
                  case Some(oid, cr, Some(ot), Some(ObservingModeType.GmosSouthLongSlit)) =>
                    session
                      .execute(Statements.selectCalibrationTargets(CalibrationTypes))(CalibrationTypes)
                      .map(targetCoordinates(ot.toInstant).map(CalibrationIdealTargets(Site.GS, ot.toInstant, _)).map(_.bestTarget(cr)))
                  case _ =>
                    none.pure[F]
               }
            // Update the target on the calibration
            _ <- (o.map(_._1), tgts).mapN { (oid, tgtid) =>
                    for {
                      ct <- Nested(targetService.cloneTargetInto(tgtid, pid)).map(_._2).value
                      _  <- ct.traverse(ct => asterismService
                              .updateAsterism(
                                Services.asSuperUser:
                                  AccessControl.unchecked(
                                    EditAsterismsPatchInput(
                                      Some(List(ct)),
                                      NonEmptyList.fromList(otgs).map(_.toList)
                                    ),
                                    List(oid),
                                    observation_id
                                  )
                              )
                            )
                    } yield ()
                }.getOrElse(Result.unit.pure[F])
            // targets may have been orphaned, delete those
            _  <- targetService.deleteOrphanCalibrationTargets(pid)
        } yield ()
      }
    }

  object Statements {

    val SetCalibrationRole: Command[(Observation.Id, Option[CalibrationRole])] =
      sql"""
        UPDATE t_observation
        SET c_calibration_role = ${calibration_role.opt}
        WHERE c_observation_id = $observation_id
      """.command.contramap((a, b) => (b, a))

    def setCalibRole(oids: List[Observation.Id], role: CalibrationRole): AppliedFragment =
      void"UPDATE t_observation " |+|
        sql"SET c_calibration_role = $calibration_role "(role) |+|
        void"WHERE c_observation_id IN (" |+|
          oids.map(sql"$observation_id").intercalate(void", ") |+| void")"

    def selectCalibrationTargets(roles: List[CalibrationRole]): Query[List[CalibrationRole], (Target.Id, String, CalibrationRole, RightAscension, Declination, Epoch, Option[Long], Option[Long], Option[RadialVelocity], Option[Parallax])] =
      sql"""SELECT
              c_target_id,
              t_target.c_name,
              t_target.c_calibration_role,
              c_sid_ra,
              c_sid_dec,
              c_sid_epoch,
              c_sid_pm_ra,
              c_sid_pm_dec,
              c_sid_rv,
              c_sid_parallax
            FROM t_target
            INNER JOIN t_program
            ON t_target.c_program_id=t_program.c_program_id
            WHERE t_program.c_calibration_role IN(${calibration_role.list(roles.length)})
              AND t_program.c_existence='present'
              AND t_target.c_existence='present'
              AND t_target.c_target_disposition = 'calibration'
            ORDER BY c_target_id
          """.query(target_id *: text *: calibration_role *: right_ascension *: declination *: epoch *: int8.opt *: int8.opt *: radial_velocity.opt *: parallax.opt)

    val selectCalibrationTimeAndConf: Query[Observation.Id, (Observation.Id, CalibrationRole, Option[Timestamp], Option[ObservingModeType])] =
      sql"""
        SELECT
            c_observation_id,
            c_calibration_role,
            c_observation_time,
            c_observing_mode_type
          FROM t_observation
          WHERE c_observation_id = $observation_id AND c_calibration_role IS NOT NULL
          """.query(observation_id *: calibration_role *: core_timestamp.opt *: observing_mode_type.opt)

    val selectActiveObservations: Query[Program.Id, Observation.Id] =
      sql"""
        SELECT
            c_observation_id
          FROM t_observation
          WHERE c_program_id = $program_id AND c_workflow_user_state IS DISTINCT FROM 'inactive'
          """.query(observation_id)

  }
}
