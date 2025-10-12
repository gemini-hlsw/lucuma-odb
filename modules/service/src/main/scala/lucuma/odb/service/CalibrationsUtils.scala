// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.MonadThrow
import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.skycalc.ImprovedSkyCalc
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.data.BlindOffsetType
import lucuma.odb.data.Nullable
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.graphql.input.ConstraintSetInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.ObservingModeInput
import lucuma.odb.graphql.input.PosAngleConstraintInput
import lucuma.odb.graphql.input.ScienceRequirementsInput
import lucuma.odb.graphql.input.SpectroscopyScienceRequirementsInput
import lucuma.odb.graphql.input.TargetEnvironmentInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs
import skunk.Transaction

import java.time.Instant
import java.time.LocalDateTime
import java.time.LocalTime

trait CalibrationTargetLocator {
  def bestTargetInList(ref: Coordinates, tgts: List[(Target.Id, String, CalibrationRole, Coordinates)]): Option[(CalibrationRole, Target.Id)] =
    tgts.minimumByOption(_._4.angularDistance(ref))(using Angle.SignedAngleOrder).map(x => (x._3, x._1))

  def idealLocation(site: Site, referenceInstant: Instant): Coordinates

  def bestTarget(site: Site, referenceInstant: Instant, role: CalibrationRole, tgts: List[(Target.Id, String, CalibrationRole, Coordinates)]): Option[(CalibrationRole, Target.Id)] =
    bestTargetInList(idealLocation(site, referenceInstant), tgts.filter(_._3 === role))
}

extension[F[_], A](r: F[Result[A]])
  def orError(using F: MonadThrow[F]): F[A]  =
    r.flatMap {
      case Result.Success(a)       => a.pure[F]
      case Result.Warning(_, a)    => a.pure[F]
      case Result.Failure(a)       => F.raiseError(new RuntimeException(a.map(_.message).toList.mkString(", ")))
      case Result.InternalError(a) => F.raiseError(a)
    }

trait SpecPhotoCalibrations extends CalibrationTargetLocator {
  def idealLocation(site: Site, referenceInstant: Instant): Coordinates = {
    val lst = ImprovedSkyCalc(site.place).getLst(referenceInstant).plusHours(1)
    val (h, m, s, n) = (lst.getHour, lst.getMinute, lst.getSecond, lst.getNano)
    val ra = RightAscension(HourAngle.fromHMS(h, m, s, 0, n / 1000))
    val dec = site.place.latitude
    Coordinates(ra, dec)
  }

  def bestTarget(site: Site, referenceInstant: Instant, tgts: List[(Target.Id, String, CalibrationRole, Coordinates)]): Option[(CalibrationRole, Target.Id)] =
    bestTarget(site, referenceInstant, CalibrationRole.SpectroPhotometric, tgts)

}

object SpecPhotoCalibrations extends SpecPhotoCalibrations

trait TwilightCalibrations extends CalibrationTargetLocator {
  def beforeMidnight(site: Site, referenceInstant: Instant): Boolean = {
    val localT = LocalDateTime.ofInstant(referenceInstant, site.timezone)
    val localNoon = LocalDateTime.of(localT.toLocalDate(), LocalTime.NOON)
    // After noon it is before the next night midnight
    // Before noon it is after the previous night midnight
    localT.isAfter(localNoon)
  }

  def idealLocation(site: Site, referenceInstant: Instant): Coordinates = {
    val lstʹ = ImprovedSkyCalc(site.place).getLst(referenceInstant)
    // X=+2 if the observation time is before local midnight, and X=-1.5 if the observation time is after local midnight.
    val lst =
      if (beforeMidnight(site, referenceInstant))
        lstʹ.plusHours(2)
      else
        lstʹ.minusHours(1).minusMinutes(30)
    val (h, m, s, n) = (lst.getHour, lst.getMinute, lst.getSecond, lst.getNano)
    val ra = RightAscension(HourAngle.fromHMS(h, m, s, 0, n / 1000))
    val dec = site.place.latitude
    Coordinates(ra, dec)
  }

  def bestTarget(site: Site, referenceInstant: Instant, tgts: List[(Target.Id, String, CalibrationRole, Coordinates)]): Option[(CalibrationRole, Target.Id)] =
    bestTarget(site, referenceInstant, CalibrationRole.Twilight, tgts)
}

object TwilightCalibrations extends TwilightCalibrations

case class CalibrationIdealTargets(
  site: Site,
  referenceInstant: Instant,
  tgts: List[(Target.Id, String, CalibrationRole, Coordinates)]
) {
  def bestSpecPhotoTarget: Option[Target.Id] =
    SpecPhotoCalibrations.bestTarget(site, referenceInstant, tgts).map(_._2)

  def bestTwilightTarget: Option[Target.Id] =
    TwilightCalibrations.bestTarget(site, referenceInstant, tgts).map(_._2)

  def bestTarget(role: CalibrationRole): Option[Target.Id] =
    role match {
      case CalibrationRole.SpectroPhotometric => bestSpecPhotoTarget
      case CalibrationRole.Twilight           => bestTwilightTarget
      case _                                  => none
    }
}

trait CalibrationObservations {
  def gmosLongSlitSpecPhotObs[F[_]: MonadThrow: Services: Transaction, G, L, U](
    pid:     Program.Id,
    gid:     Group.Id,
    tid:     Target.Id,
    props:   Map[CalibrationConfigSubset, CalibrationsService.CalObsProps],
    configs: List[CalibrationConfigSubset.Gmos[G, L, U]]
  ): F[List[Observation.Id]] =
    configs.traverse: c =>
      val matchingProps = props.find { case (originalConfig, _) =>
        // Check if this original config could have produced the transformed config using SpectroPhotometric strategy
        CalibrationConfigMatcher
          .matcherFor(originalConfig, CalibrationRole.SpectroPhotometric)
          .configsMatch(originalConfig, c)
      }.map(_._2)
      val wavelengthAt = matchingProps.flatMap(_.wavelengthAt)
      val band         = matchingProps.flatMap(_.band)
      specPhotoObservation(pid, gid, tid, wavelengthAt, band, c.toLongSlitInput)

  def roleConstraints(role: CalibrationRole) =
    role match
      case CalibrationRole.SpectroPhotometric => ConstraintSetInput.SpecPhotoCalibration
      case CalibrationRole.Twilight           => ConstraintSetInput.TwilightCalibration
      case _                                  => ConstraintSetInput.Default

  private def specPhotoObservation[F[_]: Services: MonadThrow: Transaction](
    pid:     Program.Id,
    gid:     Group.Id,
    tid:     Target.Id,
    wvAt:    Option[Wavelength],
    band:    Option[ScienceBand],
    obsMode: ObservingModeInput.Create
  ): F[Observation.Id] =
      observationService.createObservation(
        Services.asSuperUser:
          AccessControl.unchecked(
            ObservationPropertiesInput.Create.Default.copy(
              scienceBand = band,
              targetEnvironment = TargetEnvironmentInput.Create(
                none,
                List(tid).some,
                none,
                none,
                BlindOffsetType.Manual
              ).some,
              constraintSet = roleConstraints(CalibrationRole.SpectroPhotometric).some,
              group = gid.some,
              posAngleConstraint = PosAngleConstraintInput(
                mode = PosAngleConstraintMode.AverageParallactic.some, none
              ).some,
              observingMode = obsMode.some,
              scienceRequirements =
                ScienceRequirementsInput(
                  exposureTimeMode = Nullable.orNull(wvAt).map: w =>
                    ExposureTimeMode.SignalToNoiseMode(
                      SignalToNoise.unsafeFromBigDecimalExact(100.0),
                      w
                    ),
                  spectroscopy = SpectroscopyScienceRequirementsInput.Default.some,
                  imaging      = none
                ).some
              ),
              pid,
              Codecs.program_id
          )
      ).orError

  def gmosLongSlitTwilightObs[F[_]: MonadThrow: Services: Transaction, G, L, U](
    pid:     Program.Id,
    gid:     Group.Id,
    tid:     Target.Id,
    configs: List[CalibrationConfigSubset.Gmos[G, L, U]]
  ): F[List[Observation.Id]] =
    configs.traverse(c => twilightObservation(pid, gid, tid, c.centralWavelength, c.toLongSlitInput))

  private def twilightObservation[F[_]: Services: MonadThrow: Transaction](pid: Program.Id, gid: Group.Id, tid: Target.Id, cw: Wavelength, obsMode: ObservingModeInput.Create): F[Observation.Id] =
      observationService.createObservation(
        Services.asSuperUser:
          AccessControl.unchecked(
            ObservationPropertiesInput.Create.Default.copy(
              targetEnvironment = TargetEnvironmentInput.Create(
                none,
                List(tid).some,
                none,
                none,
                BlindOffsetType.Manual
              ).some,
              constraintSet = roleConstraints(CalibrationRole.Twilight).some,
              group = gid.some,
              posAngleConstraint = PosAngleConstraintInput(
                mode = PosAngleConstraintMode.Fixed.some, Angle.Angle0.some
              ).some,
              observingMode = obsMode.some,
              scienceRequirements =
                ScienceRequirementsInput(
                  exposureTimeMode = Nullable.NonNull(
                    ExposureTimeMode.SignalToNoiseMode(
                      SignalToNoise.unsafeFromBigDecimalExact(100.0),
                      cw
                    )
                  ),
                  spectroscopy = SpectroscopyScienceRequirementsInput.Default.some,
                  imaging = none
                ).some
              ),
              pid,
              Codecs.program_id
            )
      ).orError

  def flamingos2TelluricObs[F[_]: MonadThrow: Services: Transaction](
    pid: Program.Id,
    gid: Group.Id,
    tid: Target.Id,
    config: CalibrationConfigSubset.Flamingos2Configs
  ): F[Observation.Id] =
    telluricObservation(pid, gid, tid, config.toLongSlitInput)

  private def telluricObservation[F[_]: Services: MonadThrow: Transaction](
    pid: Program.Id,
    gid: Group.Id,
    tid: Target.Id,
    obsMode: ObservingModeInput.Create
  ): F[Observation.Id] =
    observationService.createObservation(
      Services.asSuperUser:
        AccessControl.unchecked(
          ObservationPropertiesInput.Create.Default.copy(
            targetEnvironment = TargetEnvironmentInput.Create(
              none,
              List(tid).some,
              none,
              none,
              BlindOffsetType.Automatic,
            ).some,
            constraintSet = roleConstraints(CalibrationRole.Telluric).some,
            group = gid.some,
            posAngleConstraint = PosAngleConstraintInput(
              mode = PosAngleConstraintMode.AverageParallactic.some, none
            ).some,
            observingMode = obsMode.some,
            scienceRequirements =
              ScienceRequirementsInput(
                exposureTimeMode = Nullable.NonNull(
                  ExposureTimeMode.SignalToNoiseMode(
                    SignalToNoise.unsafeFromBigDecimalExact(100.0),
                    Wavelength.fromIntNanometers(2000).get
                  )
                ),
                spectroscopy = SpectroscopyScienceRequirementsInput.Default.some,
                imaging = none
              ).some
            ),
            pid,
            Codecs.program_id
          )
    ).orError
}
