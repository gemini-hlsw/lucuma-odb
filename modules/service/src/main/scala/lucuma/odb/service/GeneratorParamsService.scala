// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Eq
import cats.Order
import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.data.Validated
import cats.data.ValidatedNel
import cats.derived.*
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.RadialVelocity
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.itc.client.GmosFpu
import lucuma.itc.client.ImagingIntegrationTimeParameters
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.InstrumentMode.GmosNorthSpectroscopy
import lucuma.itc.client.InstrumentMode.GmosSouthSpectroscopy
import lucuma.itc.client.SpectroscopyIntegrationTimeParameters
import lucuma.itc.client.TargetInput
import lucuma.odb.json.sourceprofile.given
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ItcInput
import lucuma.odb.sequence.data.MissingParam
import lucuma.odb.sequence.data.MissingParamSet
import lucuma.odb.sequence.gmos.longslit.Acquisition
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.circe.codec.json.*
import skunk.implicits.*

import GeneratorParamsService.Error
import Services.Syntax.*

enum ObservationSelection derives Order:
  case All
  case Science
  case Calibration

trait GeneratorParamsService[F[_]] {

  def selectOne(
    programId:     Program.Id,
    observationId: Observation.Id
  )(using Transaction[F]): F[Either[Error, GeneratorParams]]

  def selectMany(
    programId:      Program.Id,
    observationIds: List[Observation.Id]
  )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]]

  def selectMany(
    observationIds: List[Observation.Id]
  )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, Either[Error, GeneratorParams]]]

  def selectAll(
    programId: Program.Id,
    // minStatus: ObsStatus = ObsStatus.New,
    selection: ObservationSelection = ObservationSelection.All
  )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]]

}

object GeneratorParamsService {

  sealed trait Error extends Product with Serializable:
    def format: String

  object Error:
    case class MissingObservation(programId: Program.Id, observationId: Observation.Id) extends Error:
      def format: String =
        s"Observation '$observationId' in program '$programId' not found."

    case class MissingData(params: MissingParamSet) extends Error:
      def format: String = params.format

    case object ConflictingData extends Error:
      def format: String =
        "Conflicting data, all stars in the asterism must use the same observing mode and parameters."

    given Eq[Error] with
      def eqv(x: Error, y: Error): Boolean =
        (x, y) match
          case (MissingObservation(p0, o0), MissingObservation(p1, o1)) => (p0 === p1) && (o0 === o1)
          case (MissingData(p0), MissingData(p1))                       => p0 === p1
          case (ConflictingData, ConflictingData)                       => true
          case _                                                        => false

  extension (mode: InstrumentMode)
    def asImaging(λ: Wavelength): InstrumentMode =
      mode match {
        case GmosNorthSpectroscopy(_, _, _, _, _, _) =>
          InstrumentMode.GmosNorthImaging(Acquisition.filter(GmosNorthFilter.acquisition, λ, _.wavelength), none)
        case GmosSouthSpectroscopy(_, _, _, _, _, _) =>
          InstrumentMode.GmosSouthImaging(Acquisition.filter(GmosSouthFilter.acquisition, λ, _.wavelength), none)
        case x => x
      }

  def instantiate[F[_]: Concurrent](using Services[F]): GeneratorParamsService[F] =
    new GeneratorParamsService[F] {

      import lucuma.odb.sequence.gmos

      override def selectOne(
        pid: Program.Id,
        oid: Observation.Id
      )(using Transaction[F]): F[Either[Error, GeneratorParams]] =
        selectMany(pid, List(oid)).map(_.getOrElse(oid, Error.MissingObservation(pid, oid).asLeft))

      override def selectMany(
        pid:  Program.Id,
        oids: List[Observation.Id]
      )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]] =
        doSelect(selectManyParams(pid, oids))

      override def selectMany(
        oids: List[Observation.Id]
      )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, Either[Error, GeneratorParams]]] =
        doSelect(selectManyParams(oids))

      override def selectAll(
        pid:       Program.Id,
        // minStatus: ObsStatus,
        selection: ObservationSelection
      )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]] =
        doSelect(selectAllParams(pid, /*minStatus,*/ selection))

      private def doSelect(
        params: F[List[ParamsRow]]
      )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]] =
        for
          paramsRows <- params
          oms         = paramsRows.collect { case ParamsRow(oid, _, _, _, _, Some(om), _, _, _) => (oid, om) }.distinct
          m          <- observingModeServices.selectObservingMode(oms)
        yield
          NonEmptyList.fromList(paramsRows).fold(Map.empty): paramsRowsNel =>
            ObsParams.fromParamsRows(paramsRowsNel).map: (obsId, obsParams) =>
              obsId -> toObsGeneratorParams(obsParams, m.get(obsId))

      private def selectManyParams(
        pid:  Program.Id,
        oids: List[Observation.Id]
      ): F[List[ParamsRow]] =
        NonEmptyList
          .fromList(oids)
          .fold(List.empty[ParamsRow].pure[F]) { oids =>
            executeSelect(Statements.selectManyParams(user, pid, oids))
          }

      private def selectManyParams(
        oids: List[Observation.Id]
      )(using SuperUserAccess): F[List[ParamsRow]] =
        NonEmptyList
          .fromList(oids)
          .fold(List.empty[ParamsRow].pure[F]) { oids =>
            executeSelect(Statements.selectManyParams(oids))
          }

      private def selectAllParams(
        pid:       Program.Id,
        // minStatus: ObsStatus,
        selection: ObservationSelection
      ): F[List[ParamsRow]] =
        executeSelect(Statements.selectAllParams(user, pid, /*minStatus,*/ selection))

      private def executeSelect(af: AppliedFragment): F[List[ParamsRow]] =
        session
          .prepareR(af.fragment.query(Statements.params))
          .use(_.stream(af.argument, chunkSize = 64).compile.to(List))

      private def observingMode(
        params: NonEmptyList[TargetParams],
        config: Option[SourceProfile => ObservingMode]
      ): Either[Error, ObservingMode] =
        val configs: EitherNel[MissingParam, NonEmptyList[ObservingMode]] =
          params.traverse: p =>
            for
              t <- p.targetId.toRightNel(MissingParam.forObservation("target"))
              s <- p.sourceProfile.toRightNel(MissingParam.forTarget(t, "source profile"))
              f <- config.toRightNel(MissingParam.forObservation("observing mode"))
            yield f(s)

        // All of the `ObservingMode`s that we compute have to be the same.
        // Otherwise we would need to configure the instrument differently for
        // different stars in the asterism.
        configs
          .leftMap(nel => Error.MissingData(MissingParamSet.fromParams(nel)))
          .flatMap: modes =>
            ObservingMode.reconcile(modes).fold(Error.ConflictingData.asLeft)(_.asRight)

      private def toObsGeneratorParams(
        obsParams: ObsParams,
        config:    Option[SourceProfile => ObservingMode]
      ): Either[Error, GeneratorParams] =
        observingMode(obsParams.targets, config).map:
          case gn @ gmos.longslit.Config.GmosNorth(g, f, u, cw, _, _, _, _, _, _, _, _, _) =>
            val mode = InstrumentMode.GmosNorthSpectroscopy(
              cw,
              g,
              f,
              GmosFpu.North.builtin(u),
              gn.ccdMode.some,
              gn.roi.some
            )
            GeneratorParams(itcObsParams(obsParams, mode), gn, obsParams.calibrationRole)
          case gs @ gmos.longslit.Config.GmosSouth(g, f, u, cw, _, _, _, _, _, _, _, _, _) =>
            val mode = InstrumentMode.GmosSouthSpectroscopy(
              cw,
              g,
              f,
              GmosFpu.South.builtin(u),
              gs.ccdMode.some,
              gs.roi.some
            )
            GeneratorParams(itcObsParams(obsParams, mode), gs, obsParams.calibrationRole)

      private def itcObsParams(
        obsParams:  ObsParams,
        mode:       InstrumentMode,
      ): Either[MissingParamSet, ItcInput] =
        (obsParams.signalToNoise.toValidNel(MissingParam.forObservation("signal to noise")),
         obsParams.signalToNoiseAt.toValidNel(MissingParam.forObservation("signal to noise at wavelength")),
         obsParams.targets.traverse(itcTargetParams)
        ).mapN { case (s2n, λ, targets) =>
          ItcInput(
            ImagingIntegrationTimeParameters(
              λ,
              Acquisition.AcquisitionSN,
              obsParams.constraints,
              mode.asImaging(λ)
            ),
            SpectroscopyIntegrationTimeParameters(
              λ,
              s2n,
              obsParams.constraints,
              mode
            ),
            targets
          )
        }
        .leftMap(MissingParamSet.fromParams)
        .toEither

      private def itcTargetParams(targetParams: TargetParams): ValidatedNel[MissingParam, (Target.Id, TargetInput)] = {
        val sourceProf   = targetParams.sourceProfile.map(_.gaiaFree)
        val brightnesses = 
          sourceProf.flatMap: sp =>
            SourceProfile.integratedBrightnesses.getOption(sp).orElse(SourceProfile.surfaceBrightnesses.getOption(sp))

        targetParams.targetId.toValidNel(MissingParam.forObservation("target")).andThen: tid =>
          (sourceProf.toValidNel(MissingParam.forTarget(tid, "source profile")),
           Validated.condNel(brightnesses.exists(_.nonEmpty), (), MissingParam.forTarget(tid, "brightness measure")),
           targetParams.radialVelocity.toValidNel(MissingParam.forTarget(tid, "radial velocity"))
          ).mapN: (sp, _, rv) =>
            tid -> TargetInput(sp, rv)
      }

    }

  case class ParamsRow(
    observationId:   Observation.Id,
    calibrationRole: Option[CalibrationRole],
    constraints:     ConstraintSet,
    signalToNoise:   Option[SignalToNoise],
    signalToNoiseAt: Option[Wavelength],
    observingMode:   Option[ObservingModeType],
    targetId:        Option[Target.Id],
    radialVelocity:  Option[RadialVelocity],
    sourceProfile:   Option[SourceProfile]
  )

  case class TargetParams(
    targetId:        Option[Target.Id],
    radialVelocity:  Option[RadialVelocity],
    sourceProfile:   Option[SourceProfile]
  )

  case class ObsParams(
    observationId:   Observation.Id,
    calibrationRole: Option[CalibrationRole],
    constraints:     ConstraintSet,
    signalToNoise:   Option[SignalToNoise],
    signalToNoiseAt: Option[Wavelength],
    observingMode:   Option[ObservingModeType],
    targets:         NonEmptyList[TargetParams]
  )

  object ObsParams {
    def fromParamsRows(ps: NonEmptyList[ParamsRow]): Map[Observation.Id, ObsParams] =
      ps.groupBy(_.observationId).view.mapValues: oParams =>
        ObsParams(
          oParams.head.observationId,
          oParams.head.calibrationRole,
          oParams.head.constraints,
          oParams.head.signalToNoise,
          oParams.head.signalToNoiseAt,
          oParams.head.observingMode,
          oParams.map: r =>
            TargetParams(r.targetId, r.radialVelocity, r.sourceProfile)
        )
      .toMap
  }



  object Statements {

    import ProgramService.Statements.existsUserAccess

    private val source_profile: Decoder[SourceProfile] =
      jsonb.emap { sp =>
        sp.as[SourceProfile].leftMap(f => s"Could not decode SourceProfile: ${f.message}")
      }

    val params: Decoder[ParamsRow] =
      (observation_id          *:
       calibration_role.opt    *:
       constraint_set          *:
       signal_to_noise.opt     *:
       wavelength_pm.opt       *:
       observing_mode_type.opt *:
       target_id.opt           *:
       radial_velocity.opt     *:
       source_profile.opt
      ).to[ParamsRow]

    private def ParamColumns(tab: String): String =
      s"""
        $tab.c_observation_id,
        $tab.c_calibration_role,
        $tab.c_image_quality,
        $tab.c_cloud_extinction,
        $tab.c_sky_background,
        $tab.c_water_vapor,
        $tab.c_air_mass_min,
        $tab.c_air_mass_max,
        $tab.c_hour_angle_min,
        $tab.c_hour_angle_max,
        $tab.c_spec_signal_to_noise,
        $tab.c_spec_signal_to_noise_at,
        $tab.c_observing_mode_type,
        $tab.c_target_id,
        $tab.c_sid_rv,
        $tab.c_source_profile
      """

    def selectManyParams(
      user:      User,
      programId: Program.Id,
      which:     NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
          #${ParamColumns("gp")}
        FROM v_generator_params gp
        WHERE
      """(Void) |+|
        sql"""gp.c_program_id = $program_id""".apply(programId) |+|
        void""" AND gp.c_observation_id IN (""" |+|
          which.map(sql"$observation_id").intercalate(void", ") |+|
        void")" |+|
        existsUserAccess(user, programId).fold(AppliedFragment.empty) { af => void""" AND """ |+| af }

    def selectManyParams(
      which: NonEmptyList[Observation.Id]
    )(using SuperUserAccess): AppliedFragment =
      sql"""
        SELECT
          #${ParamColumns("gp")}
        FROM v_generator_params gp
        WHERE
      """(Void) |+|
        void"""gp.c_observation_id IN (""" |+|
          which.map(sql"$observation_id").intercalate(void", ") |+|
        void")"

    def selectAllParams(
      user:      User,
      programId: Program.Id,
      // minStatus: ObsStatus,
      selection: ObservationSelection
    ): AppliedFragment = {
      val selector = selection match
        case ObservationSelection.All         => void""
        case ObservationSelection.Science     => void" AND ob.c_calibration_role is null "
        case ObservationSelection.Calibration => void" AND ob.c_calibration_role is not null "

      sql"""
        SELECT
          #${ParamColumns("gp")}
        FROM v_generator_params gp
        INNER JOIN t_observation ob ON gp.c_observation_id = ob.c_observation_id
        WHERE
      """(Void) |+|
        sql"""gp.c_program_id = $program_id""".apply(programId)              |+|
        void""" AND ob.c_existence = 'present' """                           |+|
        void""" AND ob.c_workflow_user_state is distinct from 'inactive' """ |+|
        // sql""" AND ob.c_status >= $obs_status """.apply(minStatus) |+|
        // void""" AND ob.c_active_status = 'active' """              |+|
        selector                                                             |+|
        existsUserAccess(user, programId).fold(AppliedFragment.empty) { af => void""" AND """ |+| af }
    }
  }
}
