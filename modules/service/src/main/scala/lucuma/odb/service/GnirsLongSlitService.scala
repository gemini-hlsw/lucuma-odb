// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStep
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.graphql.input.GnirsLongSlitInput
import lucuma.odb.sequence.gnirs.longslit.AcquisitionConfig
import lucuma.odb.sequence.gnirs.longslit.Config
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GnirsCodecs.*
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import Services.Syntax.*

trait GnirsLongSlitService[F[_]]:
  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config]]

  def insert(
    input:  GnirsLongSlitInput.Create,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def update(
    SET:   GnirsLongSlitInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def clone(originalId: Observation.Id, newId: Observation.Id)(using Transaction[F]): F[Unit]

object GnirsLongSlitService:

  def instantiate[F[_]: {Concurrent as F, Services}]: GnirsLongSlitService[F] =

    new GnirsLongSlitService[F]:

      val gnirsLS: Decoder[Config] = (
        // Science ETM (joined from t_exposure_time_mode)
        exposure_time_mode               *:
        // Effective grating/prism/wavelength for the acquisition configuration
        gnirs_grating                    *: // c_grating_effective
        gnirs_prism                      *: // c_prism_effective
        wavelength_pm                    *: // c_grating_wavelength_effective
        // Camera
        gnirs_camera                     *:
        // FPU
        gnirs_fpu_slit                   *:
        // Filter
        gnirs_filter                     *:
        // Wavelength
        wavelength_pm                    *:
        // Coadds
        int4                             *:
        // Decker default + explicit
        gnirs_decker                     *: // c_decker_default
        gnirs_decker.opt                 *: // c_decker (explicit)
        // Read mode default + explicit
        gnirs_read_mode                  *: // c_read_mode_default
        gnirs_read_mode.opt              *: // c_read_mode (explicit)
        // Well depth default + explicit
        gnirs_well_depth                 *: // c_well_depth_default
        gnirs_well_depth.opt             *: // c_well_depth (explicit)
        // Focus motor steps (nullable = Best)
        int4.opt                         *:
        // Telescope configs: explicit (both nullable) + default
        slit_offset_mode.opt   *: // c_slit_offset_mode (explicit)
        text.opt                     *: // c_telescope_configs (explicit)
        slit_offset_mode       *: // c_slit_offset_mode_default
        text                         *: // c_telescope_configs_default
        // Acquisition inline fields
        gnirs_read_mode                  *: // c_acq_read_mode
        int4                             *: // c_acq_coadds
        gnirs_filter                     *: // c_acq_filter
        angle_µas.opt                    *: // c_acq_offset_p
        angle_µas.opt                    *: // c_acq_offset_q
        time_span                        *: // c_acq_exp_time
        int4                             *: // c_acq_exp_count
        wavelength_pm                       // c_acq_exp_at
      ).emap:
        case (sciEtm *: gratingEff *: prismEff *: gratingWavEff *:
              camera *: fpu *: filter *: centralWavelength *: coadds *:
              deckerDef *: deckerExp *:
              readModeDef *: readModeExp *:
              wellDepthDef *: wellDepthExp *:
              focusMotorSteps *:
              slitOffsetModeExp *: tcExp *: slitOffsetModeDef *: tcDef *:
              acqReadMode *: acqCoadds *: acqFilter *: acqOffP *: acqOffQ *:
              acqExpTime *: acqExpCount *: acqExpAt *: EmptyTuple) =>
          SlitTelescopeConfigsFormat.getOption((slitOffsetModeDef, tcDef))
            .toRight(s"Could not parse default telescope configs from '$tcDef'")
            .flatMap: defaultTC =>
              val explicitTC = (slitOffsetModeExp, tcExp).mapN: (mode, json) =>
                SlitTelescopeConfigsFormat.getOption((mode, json))
                  .toRight(s"Could not parse explicit telescope configs from '$json'")
              explicitTC.sequence.flatMap { explicitTCOpt =>
                PosInt.from(coadds)
                  .leftMap(e => s"Invalid coadds $coadds: $e")
                  .flatMap: coaddsP =>
                    PosInt.from(acqCoadds)
                      .leftMap(e => s"Invalid acq coadds $acqCoadds: $e")
                      .flatMap: acqCoaddsP =>
                        PosInt.from(acqExpCount)
                          .leftMap(e => s"Invalid acq exp count $acqExpCount: $e")
                          .map: acqExpCountP =>
                            val acq = AcquisitionConfig(
                              acqReadMode, acqCoaddsP, acqFilter,
                              acqOffP.map(a => Offset.P(a)),
                              acqOffQ.map(a => Offset.Q(a)),
                              acqExpTime, acqExpCountP, acqExpAt
                            )
                            val focus = focusMotorSteps.fold(GnirsFocus.Best): n =>
                              GnirsFocus.Custom(GnirsFocusMotorStepsValue.unsafeFrom(n).withUnit[GnirsFocusMotorStep])
                            Config(
                              sciEtm, gratingEff, prismEff, gratingWavEff, camera, fpu, filter, centralWavelength,
                              coaddsP,
                              deckerExp.getOrElse(deckerDef),
                              readModeExp.getOrElse(readModeDef),
                              wellDepthExp.getOrElse(wellDepthDef),
                              focus,
                              explicitTCOpt.getOrElse(defaultTC),
                              acq
                            )
              }

      override def select(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        NonEmptyList
          .fromList(which)
          .fold(Map.empty.pure[F]): oids =>
            val af = Statements.selectGnirsLongSlit(oids)
            session.prepareR(af.fragment.query(observation_id *: gnirsLS)).use: pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList.map(_.toMap)

      private def insertScienceEtm(
        input: GnirsLongSlitInput.Create,
        req: Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        exposureTimeModeService
          .insertScienceOnlyWithDefaults("GNIRS Long Slit", input.exposureTimeMode, req, which)
          .map(_.void)

      override def insert(
        input:  GnirsLongSlitInput.Create,
        req:    Option[ExposureTimeMode],
        which:  List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        (for
          _ <- ResultT(insertScienceEtm(input, req, which))
          _ <- ResultT.liftF:
            which.traverse: oid =>
              session.exec(Statements.insertGnirsLongSlit(oid, input))
            .void
        yield ()).value

      override def delete(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.deleteGnirs(which).fold(F.unit)(session.exec)

      private def updateScienceEtm(
        input: GnirsLongSlitInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        input.exposureTimeMode.fold(().pure[F]): e =>
          services.exposureTimeModeService.updateMany(which, ExposureTimeModeRole.Science, e)

      override def update(
        SET:   GnirsLongSlitInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        for
          _ <- updateScienceEtm(SET, which)
          _ <- Statements.updateGnirsLongSlit(SET, which).fold(F.unit)(session.exec)
        yield ()

      override def clone(originalId: Observation.Id, newId: Observation.Id)(using Transaction[F]): F[Unit] =
        session.exec(Statements.cloneGnirs(originalId, newId))

  object Statements:

    def selectGnirsLongSlit(observationIds: NonEmptyList[Observation.Id]): AppliedFragment =
      sql"""
        SELECT
          ls.c_observation_id,
          sci.c_exposure_time_mode,
          sci.c_signal_to_noise_at,
          sci.c_signal_to_noise,
          sci.c_exposure_time,
          sci.c_exposure_count,
          ls.c_grating_effective,
          ls.c_prism_effective,
          ls.c_grating_wavelength_effective,
          ls.c_camera,
          ls.c_fpu,
          ls.c_filter,
          ls.c_central_wavelength,
          ls.c_coadds,
          ls.c_decker_default,
          ls.c_decker,
          ls.c_read_mode_default,
          ls.c_read_mode,
          ls.c_well_depth_default,
          ls.c_well_depth,
          ls.c_focus_motor_steps,
          ls.c_slit_offset_mode,
          ls.c_telescope_configs,
          ls.c_slit_offset_mode_default,
          ls.c_telescope_configs_default,
          ls.c_acq_read_mode,
          ls.c_acq_coadds,
          ls.c_acq_filter,
          ls.c_acq_offset_p,
          ls.c_acq_offset_q,
          ls.c_acq_exp_time,
          ls.c_acq_exp_count,
          ls.c_acq_exp_at
        FROM v_gnirs_long_slit ls
        LEFT JOIN t_exposure_time_mode sci
           ON sci.c_observation_id = ls.c_observation_id
          AND sci.c_role = 'science'
      """(Void) |+|
      void"WHERE ls.c_observation_id IN (" |+|
        observationIds.map(sql"$observation_id").intercalate(void",") |+|
      void")"

    private def defaultAcqReadMode(input: GnirsLongSlitInput.Create): GnirsReadMode =
      input.acquisition.flatMap(_.readMode).getOrElse(GnirsReadMode.VeryBright)

    private def effectiveCentralWavelength(input: GnirsLongSlitInput.Create): Wavelength =
      input.centralWavelength.getOrElse(GnirsLongSlitInput.centralWavelengthFromFilter(input.filter))

    private def defaultAcqFilter(input: GnirsLongSlitInput.Create): lucuma.core.enums.GnirsFilter =
      input.acquisition.flatMap(_.filter).getOrElse(input.filter)

    private def defaultAcqExpTime(input: GnirsLongSlitInput.Create): lucuma.core.util.TimeSpan =
      input.acquisition.flatMap(_.exposureTimeMode).map(_.time)
        .getOrElse(lucuma.core.util.TimeSpan.fromSeconds(1).get)

    private def defaultAcqExpCount(input: GnirsLongSlitInput.Create): PosInt =
      input.acquisition.flatMap(_.exposureTimeMode).map(_.count)
        .getOrElse(PosInt.unsafeFrom(1))

    private def defaultAcqExpAt(input: GnirsLongSlitInput.Create): Wavelength =
      input.acquisition.flatMap(_.exposureTimeMode).map(_.at)
        .getOrElse(effectiveCentralWavelength(input))

    val InsertGnirsLongSlit: Fragment[(
      Observation.Id,
      // initial mirror
      lucuma.core.enums.GnirsGrating,
      lucuma.core.enums.GnirsPrism,
      // camera/fpu/wavelength/filter
      lucuma.core.enums.GnirsCamera,
      lucuma.core.enums.GnirsFpuSlit,
      Wavelength,
      lucuma.core.enums.GnirsFilter,
      // coadds
      Int,
      // explicit overrides (all nullable)
      Option[lucuma.core.enums.GnirsDecker],
      Option[Wavelength],
      Option[lucuma.core.enums.GnirsGrating],  // explicit grating
      Option[lucuma.core.enums.GnirsPrism],    // explicit prism
      Option[Int],         // focus_motor_steps
      Option[lucuma.core.enums.GnirsReadMode],
      Option[lucuma.core.enums.GnirsWellDepth],
      // telescope configs (nullable = use default)
      Option[SlitOffsetMode],
      Option[String],
      // acquisition
      lucuma.core.enums.GnirsReadMode,
      Int,
      lucuma.core.enums.GnirsFilter,
      Option[Long],        // acq_offset_p in µas
      Option[Long],        // acq_offset_q in µas
      lucuma.core.util.TimeSpan,
      Int,
      Wavelength
    )] =
      sql"""
        INSERT INTO t_gnirs_long_slit (
          c_observation_id,
          c_program_id,
          c_initial_grating,
          c_initial_prism,
          c_camera,
          c_initial_camera,
          c_fpu,
          c_initial_fpu,
          c_central_wavelength,
          c_initial_central_wavelength,
          c_filter,
          c_initial_filter,
          c_coadds,
          c_decker,
          c_grating_wavelength,
          c_grating,
          c_prism,
          c_focus_motor_steps,
          c_read_mode,
          c_well_depth,
          c_slit_offset_mode,
          c_telescope_configs,
          c_acq_read_mode,
          c_acq_coadds,
          c_acq_filter,
          c_acq_offset_p,
          c_acq_offset_q,
          c_acq_exp_time,
          c_acq_exp_count,
          c_acq_exp_at
        )
        SELECT
          $observation_id,
          c_program_id,
          $gnirs_grating,
          $gnirs_prism,
          $gnirs_camera,
          $gnirs_camera,
          $gnirs_fpu_slit,
          $gnirs_fpu_slit,
          $wavelength_pm,
          $wavelength_pm,
          $gnirs_filter,
          $gnirs_filter,
          $int4,
          ${gnirs_decker.opt},
          ${wavelength_pm.opt},
          ${gnirs_grating.opt},
          ${gnirs_prism.opt},
          ${int4.opt},
          ${gnirs_read_mode.opt},
          ${gnirs_well_depth.opt},
          ${slit_offset_mode.opt},
          ${text.opt},
          $gnirs_read_mode,
          $int4,
          $gnirs_filter,
          ${int8.opt},
          ${int8.opt},
          $time_span,
          $int4,
          $wavelength_pm
        FROM t_observation
        WHERE c_observation_id = $observation_id
      """.contramap {
        (oid, initGrating, initPrism, camera, fpu, wavelength, filter, coadds,
         decker, gratingWav, explGrating, explPrism, focus,
         readMode, wellDepth, slitMode, offsets,
         acqRM, acqCoadds, acqFilter, acqOffP, acqOffQ, acqExpTime, acqExpCount, acqExpAt) =>
          (oid, initGrating, initPrism, camera, camera, fpu, fpu, wavelength, wavelength,
           filter, filter, coadds, decker, gratingWav,
           explGrating, explPrism, focus, readMode, wellDepth,
           slitMode, offsets,
           acqRM, acqCoadds, acqFilter, acqOffP, acqOffQ,
           acqExpTime, acqExpCount, acqExpAt, oid)
      }

    def insertGnirsLongSlit(
      observationId: Observation.Id,
      input: GnirsLongSlitInput.Create
    ): AppliedFragment =
      val explicitTC = input.telescopeConfigs.map(SlitTelescopeConfigsFormat.reverseGet)
      val acqOffP = input.acquisition.flatMap(_.offset).map(o => Angle.microarcseconds.get(o.p.toAngle))
      val acqOffQ = input.acquisition.flatMap(_.offset).map(o => Angle.microarcseconds.get(o.q.toAngle))
      InsertGnirsLongSlit.apply(
        observationId,
        input.grating,
        input.prism,
        input.camera,
        input.fpu,
        effectiveCentralWavelength(input),
        input.filter,
        input.coadds.map(_.value).getOrElse(1),
        input.explicitDecker,
        input.explicitGratingWavelength,
        input.explicitGrating,
        input.explicitPrism,
        input.explicitFocusMotorSteps,
        input.explicitReadMode,
        input.explicitWellDepth,
        explicitTC.map(_._1),
        explicitTC.map(_._2),
        defaultAcqReadMode(input),
        input.acquisition.flatMap(_.coadds).map(_.value).getOrElse(1),
        defaultAcqFilter(input),
        acqOffP,
        acqOffQ,
        defaultAcqExpTime(input),
        defaultAcqExpCount(input).value,
        defaultAcqExpAt(input)
      )

    def deleteGnirs(which: List[Observation.Id]): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map: oids =>
        void"DELETE FROM ONLY t_gnirs_long_slit " |+|
          void"WHERE " |+| observationIdIn(oids)

    private def gnirsUpdates(SET: GnirsLongSlitInput.Edit): Option[NonEmptyList[AppliedFragment]] =
      val upCoadds       = sql"c_coadds             = ${int4_pos.opt}"
      val upWavelength   = sql"c_central_wavelength = ${wavelength_pm.opt}"
      val upFilter       = sql"c_filter             = ${gnirs_filter.opt}"
      val upFpu          = sql"c_fpu                = ${gnirs_fpu_slit.opt}"
      val upCamera       = sql"c_camera             = ${gnirs_camera.opt}"
      val upDecker       = sql"c_decker             = ${gnirs_decker.opt}"
      val upGratingWav   = sql"c_grating_wavelength = ${wavelength_pm.opt}"
      val upFocus        = sql"c_focus_motor_steps  = ${int4.opt}"
      val upReadMode     = sql"c_read_mode          = ${gnirs_read_mode.opt}"
      val upWellDepth    = sql"c_well_depth         = ${gnirs_well_depth.opt}"
      val upGrating      = sql"c_grating            = ${gnirs_grating.opt}"
      val upPrism        = sql"c_prism              = ${gnirs_prism.opt}"
      val upSlitMode     = sql"c_slit_offset_mode   = ${slit_offset_mode.opt}"
      val upOffsets      = sql"c_telescope_configs  = ${text.opt}"

      val upTelescope: Option[List[AppliedFragment]] =
        SET.telescopeConfigs.map: tc =>
          val (mode, off) = SlitTelescopeConfigsFormat.reverseGet(tc)
          List(upSlitMode(Some(mode)), upOffsets(Some(off)))

      val ups: List[AppliedFragment] = List(
        SET.coadds.toOptionOption.map(upCoadds),
        SET.centralWavelength.map(w => upWavelength(Some(w))),
        SET.filter.map(f => upFilter(Some(f))),
        SET.fpu.map(f => upFpu(Some(f))),
        SET.camera.map(c => upCamera(Some(c))),
        SET.explicitDecker.toOptionOption.map(upDecker),
        SET.explicitGratingWavelength.toOptionOption.map(upGratingWav),
        SET.explicitFocusMotorSteps.toOptionOption.map(upFocus),
        SET.explicitReadMode.toOptionOption.map(upReadMode),
        SET.explicitWellDepth.toOptionOption.map(upWellDepth),
        SET.explicitGrating.toOptionOption.map(upGrating),
        SET.explicitPrism.toOptionOption.map(upPrism)
      ).flatten ++ upTelescope.toList.flatten

      NonEmptyList.fromList(ups)

    def updateGnirsLongSlit(
      SET: GnirsLongSlitInput.Edit,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      for
        us   <- gnirsUpdates(SET)
        oids <- NonEmptyList.fromList(which)
      yield
        void"UPDATE t_gnirs_long_slit " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    def cloneGnirs(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      sql"""
        INSERT INTO t_gnirs_long_slit (
          c_observation_id, c_program_id, c_observing_mode_type,
          c_grating, c_prism, c_grating_wavelength,
          c_initial_grating, c_initial_prism,
          c_camera, c_initial_camera,
          c_fpu, c_central_wavelength, c_initial_fpu, c_initial_central_wavelength,
          c_filter, c_initial_filter,
          c_coadds, c_decker, c_focus_motor_steps, c_read_mode, c_well_depth,
          c_slit_offset_mode, c_telescope_configs,
          c_acq_read_mode, c_acq_coadds, c_acq_filter,
          c_acq_offset_p, c_acq_offset_q,
          c_acq_exp_time, c_acq_exp_count, c_acq_exp_at
        )
        SELECT
          $observation_id,
          (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id),
          c_observing_mode_type,
          c_grating, c_prism, c_grating_wavelength,
          c_initial_grating, c_initial_prism,
          c_camera, c_initial_camera,
          c_fpu, c_central_wavelength, c_initial_fpu, c_initial_central_wavelength,
          c_filter, c_initial_filter,
          c_coadds, c_decker, c_focus_motor_steps, c_read_mode, c_well_depth,
          c_slit_offset_mode, c_telescope_configs,
          c_acq_read_mode, c_acq_coadds, c_acq_filter,
          c_acq_offset_p, c_acq_offset_q,
          c_acq_exp_time, c_acq_exp_count, c_acq_exp_at
        FROM t_gnirs_long_slit
        WHERE c_observation_id = $observation_id
      """.apply(newId, newId, originalId)
