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
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED

import scala.collection.immutable.SortedMap
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.odb.Config
import lucuma.odb.data.Existence
import lucuma.odb.data.GroupTree
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.EditAsterismsPatchInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.ScienceRequirementsInput
import lucuma.odb.graphql.input.SiderealInput
import lucuma.odb.graphql.input.SpectroscopyScienceRequirementsInput
import lucuma.odb.graphql.input.TargetPropertiesInput
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

      private def calibrationsGroup(pid: Program.Id, size: Int)(using Transaction[F]): F[Option[Group.Id]] =
        if (size > 0) {
          groupService(emailConfig, httpClient).selectGroups(pid).flatMap {
            case GroupTree.Root(_, c) =>
              val existing = c.collectFirst {
                case GroupTree.Branch(gid, _, _, _, Some(CalibrationsGroupName), _, _, _, true) => gid
              }
              // Create a system group for calibrations if it does not exist
              existing match {
                case Some(gid) => gid.some.pure[F]
                case None      =>
                  groupService(emailConfig, httpClient).createGroup(
                      input = CreateGroupInput(
                        programId = pid.some,
                        proposalReference = none,
                        programReference = none,
                        SET = GroupPropertiesInput.Create(
                          name = CalibrationsGroupName.some,
                          description = CalibrationsGroupName.some,
                          minimumRequired = none,
                          ordered = false,
                          minimumInterval = none,
                          maximumInterval = none,
                          parentGroupId = none,
                          parentGroupIndex = none,
                          existence = Existence.Present
                        ),
                        Nil
                      ),
                    system = true
                  ).map(_.toOption)
              }
            case _ => none.pure[F]
          }
        } else none.pure[F]

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

      private def telluricGroupName(scienceObsId: Observation.Id): NonEmptyString =
        NonEmptyString.unsafeFrom(s"F2 Telluric for ${scienceObsId.show}")

      private def findTelluricGroupForScience(
        pid: Program.Id,
        scienceObsId: Observation.Id
      )(using Transaction[F]): F[Option[Group.Id]] =
        val groupName = telluricGroupName(scienceObsId)
        groupService(emailConfig, httpClient).selectGroups(pid).map {
          case GroupTree.Root(_, children) =>
            children.collectFirst {
              case GroupTree.Branch(gid, _, _, _, Some(name), _, _, _, true) if name == groupName => gid
            }
          case _ => none
        }

      private def getOrCreateTelluricTarget(
        pid: Program.Id
      )(using Transaction[F], ServiceAccess): F[Target.Id] =
        Services.asSuperUser:
          targetService.createTarget(
            AccessControl.unchecked(
              TargetPropertiesInput.Create(
                name = NonEmptyString.unsafeFrom("Telluric Standard"),
                subtypeInfo = SiderealInput.Create(
                  ra = RightAscension.Zero,
                  dec = Declination.Zero,
                  epoch = Epoch.J2000,
                  properMotion = none,
                  radialVelocity = none,
                  parallax = none,
                  catalogInfo = none
                ),
                sourceProfile = SourceProfile.Point(
                  SpectralDefinition.BandNormalized(
                    sed = UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V).some,
                    brightnesses = SortedMap.empty
                  )
                ),
                existence = Existence.Present
              ),
              pid,
              program_id
            ),
            TargetDisposition.Calibration,
            CalibrationRole.Telluric.some
          ).orError

      private def getTelluricObservationsInGroup(
        gid: Group.Id
      )(using Transaction[F]): F[List[Observation.Id]] =
        session.execute(sql"""
          SELECT c_observation_id
          FROM t_observation
          WHERE c_group_id = $group_id
            AND c_calibration_role = $calibration_role
        """.query(observation_id))((gid, CalibrationRole.Telluric))

      private def ensureTelluricForScience(
        pid: Program.Id,
        scienceObs: ObsExtract[CalibrationConfigSubset],
        telluricTarget: Target.Id
      )(using Transaction[F], ServiceAccess): F[Option[Observation.Id]] =
        scienceObs.data match
          case f2Config: Flamingos2Configs =>
            findTelluricGroupForScience(pid, scienceObs.id).flatMap { existingGroup =>
              existingGroup match
                case Some(gid) =>
                  getTelluricObservationsInGroup(gid).flatMap { tellurics =>
                    tellurics match
                      case telluricOid :: _ =>
                        session.prepareR(sql"""
                          SELECT c_observation_id
                          FROM t_observation
                          WHERE c_observation_id = $observation_id
                        """.query(observation_id)).use { pq =>
                          pq.option(telluricOid)
                        }.flatMap {
                          case Some(_) => none.pure[F]
                          case None =>
                            flamingos2TelluricObs(pid, gid, telluricTarget, f2Config)
                              .flatTap(oid => setCalibRoleAndGroup(List(oid), CalibrationRole.Telluric))
                              .map(_.some)
                        }
                      case Nil =>
                        flamingos2TelluricObs(pid, gid, telluricTarget, f2Config)
                          .flatTap(oid => setCalibRoleAndGroup(List(oid), CalibrationRole.Telluric))
                          .map(_.some)
                  }
                case None =>
                  val groupName = telluricGroupName(scienceObs.id)
                  groupService(emailConfig, httpClient).createGroup(
                    input = CreateGroupInput(
                      programId = pid.some,
                      proposalReference = none,
                      programReference = none,
                      SET = GroupPropertiesInput.Create(
                        name = groupName.some,
                        description = none,
                        minimumRequired = none,
                        ordered = false,
                        minimumInterval = none,
                        maximumInterval = none,
                        parentGroupId = none,
                        parentGroupIndex = none,
                        existence = Existence.Present
                      ),
                      Nil
                    ),
                    system = true
                  ).flatMap {
                    case Result.Success(gid) =>
                      flamingos2TelluricObs(pid, gid, telluricTarget, f2Config)
                        .flatTap(oid => setCalibRoleAndGroup(List(oid), CalibrationRole.Telluric))
                        .map(_.some)
                    case _ => none.pure[F]
                  }
            }
          case _ => none.pure[F]

      private def generateF2TelluricCalibrations(
        pid: Program.Id,
        f2ScienceObs: List[ObsExtract[CalibrationConfigSubset]]
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] =
        for
          telluricTarget <- getOrCreateTelluricTarget(pid)
          addedOids      <- f2ScienceObs.traverse(scienceObs =>
                              ensureTelluricForScience(pid, scienceObs, telluricTarget)
                            ).map(_.flatten)
        yield addedOids

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

      private def calObsProps(
        calibConfigs: List[ObsExtract[CalibrationConfigSubset]]
      ): Map[CalibrationConfigSubset, CalObsProps] =
        calibConfigs.groupBy(_.data).map { case (k, v) =>
          val w = v.map(_.itc.map(_.spectroscopy.exposureTimeMode.at)).flattenOption match
            case Nil =>
               none[Wavelength]
            case ws  =>
              // there must be a way to sum in wavelength space :/
              val pm = ws.map(_.toPicometers.value.value).combineAll / ws.size
              PosInt.from(pm).map(Wavelength(_)).toOption
          k -> CalObsProps(w, v.map(_.band).min)
        }

      private def uniqueConfiguration(
        all: List[ObsExtract[ObservingMode]]
      ): List[CalibrationConfigSubset] = all.map(_.data.toConfigSubset).distinct


      /**
       * Check if a calibration is actually needed by any science observation.
       */
      private def isCalibrationNeeded(
        scienceConfigs: List[CalibrationConfigSubset],
        calibConfig: CalibrationConfigSubset,
        calibRole: CalibrationRole
      ): Boolean =
        scienceConfigs.exists: sciConfig =>
          CalibrationConfigMatcher.matcherFor(sciConfig, calibRole).configsMatch(sciConfig, calibConfig)

      private def calculateConfigurationsPerRole(
        uniqueSci: List[CalibrationConfigSubset],
        calibs: List[ObsExtract[CalibrationConfigSubset]]
      ): Map[CalibrationRole, List[CalibrationConfigSubset]] =
        CalibrationTypes.map { calibType =>
          val sciConfigs = uniqueSci.map(config =>
            CalibrationConfigMatcher.matcherFor(config, calibType).normalize(config)
          ).distinct
          val calibConfigs = calibs
            .filter(_.role.contains(calibType))
            .map(_.data)
            .map(config => CalibrationConfigMatcher.matcherFor(config, calibType).normalize(config))
            .distinct
          val newConfigs = sciConfigs.diff(calibConfigs)
          (calibType, newConfigs)
        }.toMap

      private def calibObservation(
        calibRole: CalibrationRole,
        site:      Site,
        pid:       Program.Id,
        gid:       Group.Id,
        props:     Map[CalibrationConfigSubset, CalObsProps],
        configs:   List[CalibrationConfigSubset],
        tid:       Target.Id
      )(using Transaction[F]): F[List[Observation.Id]] =
        calibRole match {
          case CalibrationRole.SpectroPhotometric =>
            site match {
              case Site.GN => gmosLongSlitSpecPhotObs(pid, gid, tid, props, configs.collect { case c: GmosNConfigs => c })
              case Site.GS => gmosLongSlitSpecPhotObs(pid, gid, tid, props, configs.collect { case c: GmosSConfigs => c })
            }
          case CalibrationRole.Twilight =>
            site match
              case Site.GN => gmosLongSlitTwilightObs(pid, gid, tid, configs.collect { case c: GmosNConfigs => c })
              case Site.GS => gmosLongSlitTwilightObs(pid, gid, tid, configs.collect { case c: GmosSConfigs => c })
          case _ => List.empty.pure[F]
        }

      // Set the calibration role of the observations in bulk
      private def setCalibRoleAndGroup(oids: List[Observation.Id], calibrationRole: CalibrationRole): F[Unit] =
        session.executeCommand(Statements.setCalibRole(oids, calibrationRole)).void

      private def generateGMOSLSCalibrations(
        pid:           Program.Id,
        props:         Map[CalibrationConfigSubset, CalObsProps],
        configsPerRole: Map[CalibrationRole, List[CalibrationConfigSubset]],
        gnTgt:         CalibrationIdealTargets,
        gsTgt:         CalibrationIdealTargets
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] = {
        val allConfigs = configsPerRole.values.flatten.toList
        for {
          cg   <- calibrationsGroup(pid, allConfigs.size)
          oids <- cg.map(g =>
                    configsPerRole.toList.flatTraverse { case (calibType, configs) =>
                      generateCalibrationsForType(pid, g, props, configs, calibType, gnTgt, gsTgt)
                    }
                  ).getOrElse(List.empty.pure[F])
        } yield oids
      }

      private def generateCalibrationsForType(
        pid:       Program.Id,
        gid:       Group.Id,
        props:     Map[CalibrationConfigSubset, CalObsProps],
        configs:   List[CalibrationConfigSubset],
        calibType: CalibrationRole,
        gnTgt:     CalibrationIdealTargets,
        gsTgt:     CalibrationIdealTargets
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] = {
        def newCalibs(site: Site, idealTarget: CalibrationIdealTargets): Option[F[List[Observation.Id]]] =
          idealTarget.bestTarget(calibType).map(tgtid =>
            if (configs.nonEmpty) {
              (for {
                cta <- Nested(targetService.cloneTargetInto(tgtid, pid)).map(_._2).value
                o   <- cta.traverse(calibObservation(calibType, site, pid, gid, props, configs, _))
              } yield o).orError
            } else {
              List.empty.pure[F]
            })

        val gnoCalibs = newCalibs(Site.GN, gnTgt)
        val gsoCalibs = newCalibs(Site.GS, gsTgt)

        (gnoCalibs, gsoCalibs).mapN((_, _).mapN(_ ::: _)).getOrElse(List.empty.pure[F]).flatTap { oids =>
          setCalibRoleAndGroup(oids, calibType).whenA(oids.nonEmpty)
        }
      }

      private def removeUnnecessaryCalibrations(
        scienceConfigs: List[CalibrationConfigSubset],
        calibrations:   List[ObsExtract[CalibrationConfigSubset]]
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] = {
        // Determine which calibrations are unnecessary
        val unnecessaryOids = calibrations.collect {
          case ObsExtract(oid, _, _, Some(role), config)
          if (!isCalibrationNeeded(scienceConfigs, config, role)) =>
            oid
        }

        NonEmptyList.fromList(unnecessaryOids) match {
          case Some(oids) => observationService.deleteCalibrationObservations(oids).as(oids.toList)
          case None       => List.empty.pure[F]
        }
      }

      // Update the signal to noise at wavelength for each calbiration observation depending
      // on the average wavelength of the configuration
      private def prepareCalibrationUpdates(
        calibrations: List[ObsExtract[CalibrationConfigSubset]],
        removedOids:  List[Observation.Id],
        props:        Map[CalibrationConfigSubset, CalObsProps]
      ): List[(Observation.Id, CalObsProps)] =
        calibrations
          .filterNot { o => removedOids.contains(o.id) }
          .map { o => (o.id, props.get(o.data)) }
          .collect { case (oid, Some(props)) if props.band.isDefined || props.wavelengthAt.isDefined => (oid, props) }

      private def updatePropsAt(
        calibrationUpdates: List[(Observation.Id, CalObsProps)]
      )(using Transaction[F]): F[Unit] =
        calibrationUpdates
          .traverse { (oid, props) =>
            val bandFragment = props.band.map(sql"c_science_band IS DISTINCT FROM $science_band")
            val waveFragment = props.wavelengthAt.map(sql"c_etm_signal_to_noise_at <> $wavelength_pm")
            val needsUpdate  = List(bandFragment, waveFragment).flatten.intercalate(void" OR ")

            services.observationService.updateObservations(
              Services.asSuperUser:
                AccessControl.unchecked(
                  ObservationPropertiesInput.Edit.Empty.copy(
                    scienceBand         = Nullable.orAbsent(props.band),
                    scienceRequirements = props.wavelengthAt.map: w =>
                      ScienceRequirementsInput(
                        exposureTimeMode = Nullable.NonNull(
                          ExposureTimeMode.SignalToNoiseMode(
                            SignalToNoise.unsafeFromBigDecimalExact(100.0),
                            w
                          )
                        ),
                        spectroscopy = SpectroscopyScienceRequirementsInput.Default.some,
                        imaging      = None
                      )
                  ),
                  // Important: Only update the obs that need it or it will produce a cascade of infinite updates
                  // TODO: This could be slightly optimized by grouping obs per configuration and updating in batches
                  sql"""
                    SELECT $observation_id
                      FROM t_observation
                    WHERE c_observation_id = $observation_id
                      AND c_calibration_role IS NOT NULL
                      AND ("""(oid, oid) |+| needsUpdate |+| void")"
                )
            )
          }.void

      override def calibrationTargets(roles: List[CalibrationRole], referenceInstant: Instant)
        : F[List[(Target.Id, String, CalibrationRole, Coordinates)]] =
          session.execute(Statements.selectCalibrationTargets(roles))(roles)
            .map(targetCoordinates(referenceInstant))

      def recalculateCalibrations(pid: Program.Id, referenceInstant: Instant)(using Transaction[F], ServiceAccess): F[(List[Observation.Id], List[Observation.Id])] =
        for
          // Read calibration targets
          tgts         <- calibrationTargets(CalibrationTypes, referenceInstant)
          // Actual target for GN and GS
          gsTgt         = CalibrationIdealTargets(Site.GS, referenceInstant, tgts)
          gnTgt         = CalibrationIdealTargets(Site.GN, referenceInstant, tgts)
          // List of the program's active observations
          active       <- activeObservations(pid)
          // Get all the active science observations
          allSci       <- allObservations(pid, ObservationSelection.Science)
                            .map(_.filter(u => active.contains(u._1)))

          // Separate F2 science observations from GMOS
          f2Sci         = allSci.filter(_.data.toConfigSubset.isInstanceOf[Flamingos2Configs])
          gmosSci       = allSci.filterNot(_.data.toConfigSubset.isInstanceOf[Flamingos2Configs])
          f2ScienceObs  = toConfigForCalibration(f2Sci)

          // GMOS calibrations (existing config-based logic)
          uniqueGmosSci = uniqueConfiguration(gmosSci)
          // Get all the active calibration observations (excluding those with execution events)
          allCalibs    <- allUnexecutedObservations(pid, ObservationSelection.Calibration).map(_.filter(u => active.contains(u.id)))
          calibs        = toConfigForCalibration(allCalibs)
          gmosCalibs    = calibs.filterNot(_.data.isInstanceOf[Flamingos2Configs])
          // Average s/n wavelength at each configuration
          props         = calObsProps(toConfigForCalibration(allSci))
          // Per-calibration-type configuration processing (GMOS only)
          gmosConfigsPerRole = calculateConfigurationsPerRole(uniqueGmosSci, gmosCalibs)
          // Remove unnecessary GMOS calibrations
          removedGmosOids <- removeUnnecessaryCalibrations(uniqueGmosSci, gmosCalibs)
          // Generate new GMOS calibrations
          addedGmosOids <- generateGMOSLSCalibrations(pid, props, gmosConfigsPerRole, gnTgt, gsTgt)
          // Update wavelength at for each remaining GMOS calib
          gmosCalibUpdates = prepareCalibrationUpdates(gmosCalibs, removedGmosOids, props)
          _            <- updatePropsAt(gmosCalibUpdates)

          // F2 telluric calibrations (new observation-based logic)
          addedF2Oids  <- generateF2TelluricCalibrations(pid, f2ScienceObs)

          // Cleanup
          _            <- targetService.deleteOrphanCalibrationTargets(pid)
        yield (addedGmosOids ::: addedF2Oids, removedGmosOids)

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
