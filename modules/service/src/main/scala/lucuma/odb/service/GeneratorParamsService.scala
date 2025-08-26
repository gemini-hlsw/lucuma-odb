// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import cats.syntax.functorFilter.*
import cats.syntax.option.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ScienceBand
import lucuma.core.math.RadialVelocity
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.itc.client.GmosFpu
import lucuma.itc.client.ImagingParameters
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.InstrumentMode.Flamingos2Imaging
import lucuma.itc.client.InstrumentMode.Flamingos2Spectroscopy
import lucuma.itc.client.InstrumentMode.GmosNorthImaging
import lucuma.itc.client.InstrumentMode.GmosNorthSpectroscopy
import lucuma.itc.client.InstrumentMode.GmosSouthImaging
import lucuma.itc.client.InstrumentMode.GmosSouthSpectroscopy
import lucuma.itc.client.ItcConstraintsInput.*
import lucuma.itc.client.SpectroscopyParameters
import lucuma.itc.client.TargetInput
import lucuma.odb.data.ExposureTimeModeType
import lucuma.odb.json.sourceprofile.given
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ItcInput
import lucuma.odb.sequence.data.MissingParam
import lucuma.odb.sequence.data.MissingParamSet
import lucuma.odb.sequence.flamingos2
import lucuma.odb.sequence.gmos.longslit.Acquisition
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.circe.codec.json.*
import skunk.codec.boolean.bool
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
  )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]]

  def selectAll(
    programId: Program.Id,
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
      mode match
        case Flamingos2Imaging(_)                    =>
          mode
        case Flamingos2Spectroscopy(_, f, _)         =>
          InstrumentMode.Flamingos2Imaging(lucuma.odb.sequence.flamingos2.longslit.Acquisition.toAcquisitionFilter(f))
        case GmosNorthImaging(_, _)                  =>
          mode
        case GmosNorthSpectroscopy(_, _, _, _, _, _) =>
          InstrumentMode.GmosNorthImaging(Acquisition.filter(GmosNorthFilter.acquisition, λ, _.wavelength), none)
        case GmosSouthImaging(_, _)                  =>
          mode
        case GmosSouthSpectroscopy(_, _, _, _, _, _) =>
          InstrumentMode.GmosSouthImaging(Acquisition.filter(GmosSouthFilter.acquisition, λ, _.wavelength), none)

  def instantiate[F[_]: Concurrent](using Services[F]): GeneratorParamsService[F] =
    new GeneratorParamsService[F] {

      import lucuma.odb.sequence.gmos

      val customSedIdOptional = SourceProfile.unnormalizedSED.some.andThen(UnnormalizedSED.userDefinedAttachment).andThen(UnnormalizedSED.UserDefinedAttachment.attachmentId)

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
      )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]] =
        doSelect(selectManyParams(oids))

      override def selectAll(
        pid:       Program.Id,
        selection: ObservationSelection
      )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]] =
        doSelect(selectAllParams(pid, selection))

      private def doSelect(
        params: F[List[ParamsRow]]
      )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]] =
        for
          paramsRows <- params
          oms         = paramsRows.collect { case ParamsRow(oid, _, _, _, Some(om), _, _, _, _, _, _, _) => (oid, om) }.distinct
          m          <- Services.asSuperUser(observingModeServices.selectObservingMode(oms))
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
      ): F[List[ParamsRow]] =
        NonEmptyList
          .fromList(oids)
          .fold(List.empty[ParamsRow].pure[F]) { oids =>
            executeSelect(Statements.selectManyParams(oids))
          }

      private def selectAllParams(
        pid:       Program.Id,
        selection: ObservationSelection
      ): F[List[ParamsRow]] =
        executeSelect(Statements.selectAllParams(user, pid, /*minStatus,*/ selection))

      private def executeSelect(af: AppliedFragment): F[List[ParamsRow]] =
        session
          .prepareR(af.fragment.query(Statements.params))
          .use(_.stream(af.argument, chunkSize = 64).compile.to(List))
          .flatMap(addCustomSedTimestamps)

      // If the user uploads a new custom sed in place of an existing one, that needs to
      // invalidate the cache. So, we include the timestamp of the attachment (if any) in
      // the hash.
      private def addCustomSedTimestamps(params: List[ParamsRow]): F[List[ParamsRow]] =
        NonEmptyList.fromList(params.map(p => p.sourceProfile.flatMap(customSedIdOptional.getOption)).flattenOption)
          .fold(params.pure)(attIds =>
            Services.asSuperUser(attachmentMetadataService.getUpdatedAt(attIds)).map(map =>
              params.map(p =>
                val aid = p.sourceProfile.flatMap(customSedIdOptional.getOption)
                aid.fold(p)(id => p.copy(customSedTimestamp = map.get(id)))
              )
            )
          )

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
          case gn @ gmos.longslit.Config.GmosNorth(g, f, u, c) =>
            val mode = InstrumentMode.GmosNorthSpectroscopy(
              c.centralWavelength,
              g,
              f,
              GmosFpu.North.builtin(u),
              gn.ccdMode.some,
              gn.roi.some
            )
            GeneratorParams(itcObsParams(obsParams, mode), obsParams.scienceBand, gn, obsParams.calibrationRole, obsParams.declaredComplete, obsParams.acqResetTime)

          case gs @ gmos.longslit.Config.GmosSouth(g, f, u, c) =>
            val mode = InstrumentMode.GmosSouthSpectroscopy(
              c.centralWavelength,
              g,
              f,
              GmosFpu.South.builtin(u),
              gs.ccdMode.some,
              gs.roi.some
            )
            GeneratorParams(itcObsParams(obsParams, mode), obsParams.scienceBand, gs, obsParams.calibrationRole, obsParams.declaredComplete, obsParams.acqResetTime)

          case f2 @ flamingos2.longslit.Config(disperser, filter, fpu, _, _, _, _, _, _, _) =>
            val mode = InstrumentMode.Flamingos2Spectroscopy(disperser, filter, fpu)
            GeneratorParams(itcObsParams(obsParams, mode), obsParams.scienceBand, f2, obsParams.calibrationRole, obsParams.declaredComplete, obsParams.acqResetTime)

          case gn @ gmos.imaging.Config.GmosNorth(filters = filters) =>
            // FIXME: This is not right we have n filters
            val mode = InstrumentMode.GmosNorthImaging(filters.head, gn.ccdMode.some)
            GeneratorParams(itcObsParams(obsParams, mode), obsParams.scienceBand, gn, obsParams.calibrationRole, obsParams.declaredComplete, obsParams.acqResetTime)

          case gs @ gmos.imaging.Config.GmosSouth(filters = filters) =>
            // FIXME: This is not right we have n filters
            val mode = InstrumentMode.GmosSouthImaging(filters.head, gs.ccdMode.some)
            GeneratorParams(itcObsParams(obsParams, mode), obsParams.scienceBand, gs, obsParams.calibrationRole, obsParams.declaredComplete, obsParams.acqResetTime)

      private def itcObsParams(
        obsParams:  ObsParams,
        mode:       InstrumentMode,
      ): Either[MissingParamSet, ItcInput] =
        (obsParams.exposureTimeMode.toValidNel(MissingParam.forObservation("exposure time mode")),
         obsParams.targets.traverse(itcTargetParams)
        ).mapN { case (exposureTimeMode, targets) =>
          val ici = obsParams.constraints.toInput
          ItcInput(
            ImagingParameters(
              ExposureTimeMode.SignalToNoiseMode(Acquisition.AcquisitionSN, exposureTimeMode.at),
              ici,
              mode.asImaging(exposureTimeMode.at)
            ),
            SpectroscopyParameters(
              exposureTimeMode,
              ici,
              mode
            ),
            targets
          )
        }
        .leftMap(MissingParamSet.fromParams)
        .toEither

      private def itcTargetParams(targetParams: TargetParams): ValidatedNel[MissingParam, (Target.Id, TargetInput, Option[Timestamp])] = {
        // If emission line, SED not required, otherwhise must be defined
        def hasITCRequiredSEDParam(sp: SourceProfile): Boolean =
          SourceProfile.unnormalizedSED.getOption(sp).flatten.isDefined ||
          SourceProfile.integratedEmissionLinesSpectralDefinition.getOption(sp).isDefined ||
          SourceProfile.surfaceEmissionLinesSpectralDefinition.getOption(sp).isDefined

        val sourceProf   = targetParams.sourceProfile.map(_.gaiaFree)
        val brightnesses =
          sourceProf.flatMap: sp =>
            SourceProfile.integratedBrightnesses.getOption(sp).orElse(SourceProfile.surfaceBrightnesses.getOption(sp))
              .map(_.nonEmpty)
        val wavelengthLines =
          sourceProf.flatMap: sp =>
            SourceProfile.integratedWavelengthLines.getOption(sp).orElse(SourceProfile.surfaceWavelengthLines.getOption(sp))
              .map(_.nonEmpty)
        val validBrightness = brightnesses.orElse(wavelengthLines).getOrElse(false)
        val sed = sourceProf.filter(hasITCRequiredSEDParam)
        val validCustomSed = sourceProf.flatMap(customSedIdOptional.getOption).isEmpty || targetParams.customSedTimestamp.isDefined

        targetParams.targetId.toValidNel(MissingParam.forObservation("target")).andThen: tid =>
          (sourceProf.toValidNel(MissingParam.forTarget(tid, "source profile")),
           sed.toValidNel(MissingParam.forTarget(tid, "SED")),
           Validated.condNel(validBrightness, (), MissingParam.forTarget(tid, "brightness measure")),
           Validated.condNel(validCustomSed, (), MissingParam.forTarget(tid, "custom SED attachment"))
          ).mapN: (sp,_, _, _) =>
            (tid, TargetInput(sp, targetParams.radialVelocity.getOrElse(RadialVelocity.Zero)), targetParams.customSedTimestamp)
      }

    }

  case class ParamsRow(
    observationId:      Observation.Id,
    calibrationRole:    Option[CalibrationRole],
    constraints:        ConstraintSet,
    exposureTimeMode:   Option[ExposureTimeMode],
    observingMode:      Option[ObservingModeType],
    scienceBand:        Option[ScienceBand],
    targetId:           Option[Target.Id],
    radialVelocity:     Option[RadialVelocity],
    sourceProfile:      Option[SourceProfile],
    declaredComplete:   Boolean,
    acqResetTime:       Option[Timestamp],
    customSedTimestamp: Option[Timestamp] = none
  )

  case class TargetParams(
    targetId:           Option[Target.Id],
    radialVelocity:     Option[RadialVelocity],
    sourceProfile:      Option[SourceProfile],
    customSedTimestamp: Option[Timestamp]
  )

  case class ObsParams(
    observationId:    Observation.Id,
    calibrationRole:  Option[CalibrationRole],
    constraints:      ConstraintSet,
    exposureTimeMode: Option[ExposureTimeMode],
    observingMode:    Option[ObservingModeType],
    scienceBand:      Option[ScienceBand],
    targets:          NonEmptyList[TargetParams],
    declaredComplete: Boolean,
    acqResetTime:     Option[Timestamp]
  )

  object ObsParams {
    def fromParamsRows(ps: NonEmptyList[ParamsRow]): Map[Observation.Id, ObsParams] =
      ps.groupBy(_.observationId).view.mapValues: oParams =>
        ObsParams(
          oParams.head.observationId,
          oParams.head.calibrationRole,
          oParams.head.constraints,
          oParams.head.exposureTimeMode,
          oParams.head.observingMode,
          oParams.head.scienceBand,
          oParams.map: r =>
            TargetParams(r.targetId, r.radialVelocity, r.sourceProfile, r.customSedTimestamp),
          oParams.head.declaredComplete,
          oParams.head.acqResetTime
        )
      .toMap
  }



  object Statements {

    import ProgramUserService.Statements.existsUserReadAccess

    private val source_profile: Decoder[SourceProfile] =
      jsonb.emap { sp =>
        sp.as[SourceProfile].leftMap(f => s"Could not decode SourceProfile: ${f.message}")
      }

    val exposure_time_mode: Decoder[Option[ExposureTimeMode]] =
      (
        exposure_time_mode_type.opt  *:
        wavelength_pm.opt            *:
        signal_to_noise.opt          *:
        time_span.opt                *:
        int4_pos.opt
      ).emap: (tpe, at, s2n, time, count) =>
        tpe.fold(none[ExposureTimeMode].asRight[String]) {
          case ExposureTimeModeType.SignalToNoiseMode =>
            (s2n, at)
              .mapN(ExposureTimeMode.SignalToNoiseMode.apply)
              .toRight("Both c_etm_signal_to_noise and c_etm_signal_to_noise_at must be defined for the SignalToNoise exposure time mode.")
              .map(_.some)

          case ExposureTimeModeType.TimeAndCountMode  =>
            (time, count, at)
              .mapN(ExposureTimeMode.TimeAndCountMode.apply)
              .toRight("c_etm_exp_time, c_etm_exp_count and c_etm_signal_to_noise_at must be defined for the TimeAndCount exposure time mode.")
              .map(_.some)
        }


    val params: Decoder[ParamsRow] =
      (observation_id          *:
       calibration_role.opt    *:
       constraint_set          *:
       exposure_time_mode      *:
       observing_mode_type.opt *:
       science_band.opt        *:
       target_id.opt           *:
       radial_velocity.opt     *:
       source_profile.opt      *:
       bool                    *:
       core_timestamp.opt
      ).map( (oid, role, cs, etm, om, sb, tid, rv, sp, dc, art) =>
        ParamsRow(oid, role, cs, etm, om, sb, tid, rv, sp, dc, art))

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
        $tab.c_exp_time_mode,
        $tab.c_etm_signal_to_noise_at,
        $tab.c_etm_signal_to_noise,
        $tab.c_etm_exp_time,
        $tab.c_etm_exp_count,
        $tab.c_observing_mode_type,
        $tab.c_science_band,
        $tab.c_target_id,
        $tab.c_sid_rv,
        $tab.c_source_profile,
        $tab.c_declared_complete,
        $tab.c_acq_reset_time
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
        existsUserReadAccess(user, programId).fold(AppliedFragment.empty) { af => void""" AND """ |+| af }

    def selectManyParams(
      which: NonEmptyList[Observation.Id]
    ): AppliedFragment =
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
        existsUserReadAccess(user, programId).fold(AppliedFragment.empty) { af => void""" AND """ |+| af }
    }
  }
}
