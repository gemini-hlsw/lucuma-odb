// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Monad
import cats.MonadThrow
import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservationWorkflowState
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
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.data.ItcInput
import lucuma.odb.sequence.flamingos2.longslit.Config as Flamingos2Config
import lucuma.odb.sequence.gmos.longslit.Config.GmosNorth as GmosNorthLongSlit
import lucuma.odb.sequence.gmos.longslit.Config.GmosSouth as GmosSouthLongSlit
import lucuma.odb.service.CalibrationConfigSubset.*
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs
import lucuma.odb.util.Codecs.*
import skunk.Transaction
import skunk.syntax.all.*

import java.time.Instant
import java.time.LocalDateTime
import java.time.LocalTime

case class ObsExtract[A](
  id:   Observation.Id,
  itc:  Option[ItcInput],
  band: Option[ScienceBand],
  role: Option[CalibrationRole],
  data: A
):
  def map[B](f: A => B): ObsExtract[B] =
    copy(data = f(data))

object ObsExtract:
  val PerProgramPerConfigCalibrationTypes = List(CalibrationRole.SpectroPhotometric, CalibrationRole.Twilight)

  def perObsFilter: PartialFunction[ObsExtract[ObservingMode], ObsExtract[ObservingMode]] =
    case d @ ObsExtract(data = _: Flamingos2Config) => d

  def perProgramFilter: PartialFunction[ObsExtract[ObservingMode], ObsExtract[ObservingMode]] =
    case d @ ObsExtract(data = _: GmosNorthLongSlit) => d
    case d @ ObsExtract(data = _: GmosSouthLongSlit) => d

  def perProgramCalibrationFilter: PartialFunction[ObsExtract[CalibrationConfigSubset], ObsExtract[CalibrationConfigSubset]] =
    case d @ ObsExtract(role = Some(r)) if PerProgramPerConfigCalibrationTypes.exists(_ === r) => d

case class CalObsProps(
  wavelengthAt: Option[Wavelength],
  band:         Option[ScienceBand]
)

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

trait WorkflowStateQueries[F[_]: Monad: Services] {


  def filterWorkflowStateNotIn[A](obs: List[A], oid: A => Observation.Id, states: List[ObservationWorkflowState]) =
    filterWorkflow(obs, oid, states, !_, ready = false)

  def filterWorkflowStateIn[A](obs: List[A], oid: A => Observation.Id, states: List[ObservationWorkflowState], ready: Boolean) =
    filterWorkflow(obs, oid, states, identity, ready)

  def excludeOngoingAndCompleted[A](obs: List[A], oid: A => Observation.Id): F[List[A]] =
    filterWorkflowStateNotIn(obs, oid, List(ObservationWorkflowState.Ongoing, ObservationWorkflowState.Completed))

  def onlyDefinedAndReady[A](obs: List[A], oid: A => Observation.Id): F[List[A]] =
    filterWorkflowStateIn(obs, oid, List(ObservationWorkflowState.Defined, ObservationWorkflowState.Ready), true)

  private val WorkflowStateReadyQuery =
    sql"""
      SELECT c_workflow_state
      FROM t_obscalc
      WHERE c_observation_id = $observation_id
        AND c_obscalc_state = 'ready'
    """.query(observation_workflow_state.opt)

  private val WorkflowStateAnyQuery =
    sql"""
      SELECT c_workflow_state
      FROM t_obscalc
      WHERE c_observation_id = $observation_id
    """.query(observation_workflow_state.opt)

  private def filterWorkflow[A](obs: List[A], oid: A => Observation.Id, states: List[ObservationWorkflowState], f: Boolean => Boolean, ready: Boolean) =
    val selectFn = if (ready) selectObscalcWorkflowState else selectObscalcWorkflowStateAny
    obs.filterA: obs =>
      selectFn(oid(obs)).map: calculatedState =>
        f(calculatedState.exists(s => states.exists(_ === s)))

  private def selectWorkflowState(oid: Observation.Id, onlyReady: Boolean): F[Option[ObservationWorkflowState]] =
    session.option(if (onlyReady) WorkflowStateReadyQuery else WorkflowStateAnyQuery)(oid).map(_.flatten)

  def selectObscalcWorkflowState(oid: Observation.Id): F[Option[ObservationWorkflowState]] =
    selectWorkflowState(oid, onlyReady = true)

  def selectObscalcWorkflowStateAny(oid: Observation.Id): F[Option[ObservationWorkflowState]] =
    selectWorkflowState(oid, onlyReady = false)
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
  def toConfigForCalibration(all: List[ObsExtract[ObservingMode]]): List[ObsExtract[CalibrationConfigSubset]] =
    all.map(_.map(_.toConfigSubset))

  def gmosLongSlitSpecPhotObs[F[_]: MonadThrow: Services: Transaction, G, L, U](
    pid:    Program.Id,
    gid:    Group.Id,
    tid:    Target.Id,
    props:  Map[CalibrationConfigSubset, CalObsProps],
    config: CalibrationConfigSubset.Gmos[G, L, U]
  ): F[Observation.Id] =
    val matchingProps = props.find { case (originalConfig, _) =>
      // Check if this original config could have produced the transformed config using SpectroPhotometric strategy
      CalibrationConfigMatcher
        .matcherFor(originalConfig, CalibrationRole.SpectroPhotometric)
        .configsMatch(originalConfig, config)
    }.map(_._2)
    val wavelengthAt = matchingProps.flatMap(_.wavelengthAt)
    val band         = matchingProps.flatMap(_.band)
    specPhotoObservation(pid, gid, tid, wavelengthAt, band, config.toLongSlitInput)

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
    pid:    Program.Id,
    gid:    Group.Id,
    tid:    Target.Id,
    config: CalibrationConfigSubset.Gmos[G, L, U]
  ): F[Observation.Id] =
    twilightObservation(pid, gid, tid, config.centralWavelength, config.toLongSlitInput)

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
}
