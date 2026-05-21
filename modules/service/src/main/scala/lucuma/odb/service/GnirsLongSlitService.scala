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
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsObsReadMode
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStep
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.odb.data.ExposureTimeModeId
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

  def clone(
    originalId: Observation.Id,
    newId:      Observation.Id,
    etms:       List[(ExposureTimeModeId, ExposureTimeModeId)]
  )(using Transaction[F]): F[Unit]

object GnirsLongSlitService:

  def instantiate[F[_]: {Concurrent as F, Services}]: GnirsLongSlitService[F] =

    new GnirsLongSlitService[F]:

      val gnirsLS: Decoder[Config] = (
        // Science ETM (joined from t_exposure_time_mode via FK)
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
        gnirs_obs_read_mode              *: // c_read_mode_default
        gnirs_obs_read_mode.opt          *: // c_read_mode (explicit)
        // Well depth default + explicit
        gnirs_well_depth                 *: // c_well_depth_default
        gnirs_well_depth.opt             *: // c_well_depth (explicit)
        // Focus motor steps (nullable = Best)
        int4.opt                         *:
        // Telescope configs: explicit (both nullable) + default
        slit_offset_mode.opt             *: // c_slit_offset_mode (explicit)
        text.opt                         *: // c_telescope_configs (explicit)
        slit_offset_mode                 *: // c_slit_offset_mode_default
        text                             *: // c_telescope_configs_default
        // Acquisition fields (inline cols + acq ETM joined from t_exposure_time_mode via FK)
        gnirs_obs_read_mode              *: // c_acq_read_mode
        int4                             *: // c_acq_coadds
        gnirs_filter                     *: // c_acq_filter
        angle_µas.opt                    *: // c_acq_offset_p
        angle_µas.opt                    *: // c_acq_offset_q
        exposure_time_mode                  // acquisition ETM
      ).emap:
        case (sciEtm *: gratingEff *: prismEff *: gratingWavEff *:
              camera *: fpu *: filter *: centralWavelength *: coadds *:
              deckerDef *: deckerExp *:
              readModeDef *: readModeExp *:
              wellDepthDef *: wellDepthExp *:
              focusMotorSteps *:
              slitOffsetModeExp *: tcExp *: slitOffsetModeDef *: tcDef *:
              acqReadMode *: acqCoadds *: acqFilter *: acqOffP *: acqOffQ *:
              acqEtm *: EmptyTuple) =>
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
                      .map: acqCoaddsP =>
                        val acq = AcquisitionConfig(
                          acqReadMode, acqCoaddsP, acqFilter,
                          acqOffP.map(a => Offset.P(a)),
                          acqOffQ.map(a => Offset.Q(a)),
                          acqEtm
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

      private def insertExposureTimeModes(
        input: GnirsLongSlitInput.Create,
        req:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Map[Observation.Id, (ExposureTimeModeId, ExposureTimeModeId)]]] =
        val acqEtm: Option[ExposureTimeMode] = input.acquisition.flatMap(_.exposureTimeMode)
        exposureTimeModeService
          .insertOneWithDefaults("GNIRS Long Slit", acqEtm, input.exposureTimeMode, req, which)

      override def insert(
        input:  GnirsLongSlitInput.Create,
        req:    Option[ExposureTimeMode],
        which:  List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        (for
          ids <- ResultT(insertExposureTimeModes(input, req, which))
          _   <- ResultT.liftF:
            which.traverse: oid =>
              val (acqId, sciId) = ids(oid)
              session.exec(Statements.insertGnirsLongSlit(oid, acqId, sciId, input))
            .void
        yield ()).value

      override def delete(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.deleteGnirs(which).fold(F.unit)(session.exec)

      private def updateExposureTimeModes(
        input: GnirsLongSlitInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        def update(etm: Option[ExposureTimeMode], role: ExposureTimeModeRole): F[Unit] =
          etm.fold(().pure[F]): e =>
            services.exposureTimeModeService.updateMany(which, role, e)
        for
          _ <- update(input.acquisition.flatMap(_.exposureTimeMode), ExposureTimeModeRole.Acquisition)
          _ <- update(input.exposureTimeMode, ExposureTimeModeRole.Science)
        yield ()

      override def update(
        SET:   GnirsLongSlitInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        for
          _ <- updateExposureTimeModes(SET, which)
          _ <- Statements.updateGnirsLongSlit(SET, which).fold(F.unit)(session.exec)
        yield ()

      override def clone(
        originalId: Observation.Id,
        newId:      Observation.Id,
        etms:       List[(ExposureTimeModeId, ExposureTimeModeId)]
      )(using Transaction[F]): F[Unit] =
        session.exec(Statements.cloneGnirs(originalId, newId, etms))

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
          acq.c_exposure_time_mode,
          acq.c_signal_to_noise_at,
          acq.c_signal_to_noise,
          acq.c_exposure_time,
          acq.c_exposure_count
        FROM v_gnirs_long_slit ls
        INNER JOIN t_exposure_time_mode sci
           ON sci.c_exposure_time_mode_id = ls.c_science_exposure_time_mode_id
        INNER JOIN t_exposure_time_mode acq
           ON acq.c_exposure_time_mode_id = ls.c_acquisition_exposure_time_mode_id
      """(Void) |+|
      void"WHERE ls.c_observation_id IN (" |+|
        observationIds.map(sql"$observation_id").intercalate(void",") |+|
      void")"

    private def defaultAcqReadMode(input: GnirsLongSlitInput.Create): GnirsObsReadMode =
      input.acquisition.flatMap(_.readMode).getOrElse(GnirsObsReadMode.AutomaticInEachStep)

    private def effectiveCentralWavelength(input: GnirsLongSlitInput.Create): Wavelength =
      input.centralWavelength.getOrElse(GnirsLongSlitInput.centralWavelengthFromFilter(input.filter))

    private def defaultAcqFilter(input: GnirsLongSlitInput.Create): GnirsFilter =
      input.acquisition.flatMap(_.filter).getOrElse(input.filter)

    val InsertGnirsLongSlit: Fragment[(
      Observation.Id,
      // initial mirror
      GnirsGrating,
      GnirsPrism,
      // camera/fpu/wavelength/filter
      GnirsCamera,
      GnirsFpuSlit,
      Wavelength,
      GnirsFilter,
      // coadds
      Int,
      // explicit overrides (all nullable)
      Option[GnirsDecker],
      Option[Wavelength],
      Option[GnirsGrating],   // explicit grating
      Option[GnirsPrism],     // explicit prism
      Option[Int],            // focus_motor_steps
      Option[GnirsObsReadMode],
      Option[GnirsWellDepth],
      // telescope configs (nullable = use default)
      Option[SlitOffsetMode],
      Option[String],
      // acquisition inline columns
      GnirsObsReadMode,
      Int,
      GnirsFilter,
      Option[Long],           // acq_offset_p in µas
      Option[Long],           // acq_offset_q in µas
      // exposure time mode FKs
      ExposureTimeModeId,     // science
      ExposureTimeModeId      // acquisition
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
          c_science_exposure_time_mode_id,
          c_acquisition_exposure_time_mode_id
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
          ${gnirs_obs_read_mode.opt},
          ${gnirs_well_depth.opt},
          ${slit_offset_mode.opt},
          ${text.opt},
          $gnirs_obs_read_mode,
          $int4,
          $gnirs_filter,
          ${int8.opt},
          ${int8.opt},
          $exposure_time_mode_id,
          $exposure_time_mode_id
        FROM t_observation
        WHERE c_observation_id = $observation_id
      """.contramap {
        (oid, initGrating, initPrism, camera, fpu, wavelength, filter, coadds,
         decker, gratingWav, explGrating, explPrism, focus,
         readMode, wellDepth, slitMode, offsets,
         acqRM, acqCoadds, acqFilter, acqOffP, acqOffQ,
         sciEtmId, acqEtmId) =>
          (oid, initGrating, initPrism, camera, camera, fpu, fpu, wavelength, wavelength,
           filter, filter, coadds, decker, gratingWav,
           explGrating, explPrism, focus, readMode, wellDepth,
           slitMode, offsets,
           acqRM, acqCoadds, acqFilter, acqOffP, acqOffQ,
           sciEtmId, acqEtmId, oid)
      }

    def insertGnirsLongSlit(
      observationId: Observation.Id,
      acqEtmId:      ExposureTimeModeId,
      sciEtmId:      ExposureTimeModeId,
      input:         GnirsLongSlitInput.Create
    ): AppliedFragment =
      val explicitTC = input.explicitTelescopeConfigs.map(SlitTelescopeConfigsFormat.reverseGet)
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
        sciEtmId,
        acqEtmId
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
      val upReadMode     = sql"c_read_mode          = ${gnirs_obs_read_mode.opt}"
      val upWellDepth    = sql"c_well_depth         = ${gnirs_well_depth.opt}"
      val upGrating      = sql"c_grating            = ${gnirs_grating.opt}"
      val upPrism        = sql"c_prism              = ${gnirs_prism.opt}"
      val upSlitMode     = sql"c_slit_offset_mode   = ${slit_offset_mode.opt}"
      val upOffsets      = sql"c_telescope_configs  = ${text.opt}"

      // Acquisition inline (non-ETM) column updates
      val upAcqReadMode  = sql"c_acq_read_mode = $gnirs_obs_read_mode"
      val upAcqCoadds    = sql"c_acq_coadds    = $int4_pos"
      val upAcqFilter    = sql"c_acq_filter    = $gnirs_filter"
      val upAcqOffsetP   = sql"c_acq_offset_p  = ${int8.opt}"
      val upAcqOffsetQ   = sql"c_acq_offset_q  = ${int8.opt}"

      val upTelescope: Option[List[AppliedFragment]] =
        SET.explicitTelescopeConfigs.toOptionOption.map:
          case Some(tc) =>
            val (mode, off) = SlitTelescopeConfigsFormat.reverseGet(tc)
            List(upSlitMode(Some(mode)), upOffsets(Some(off)))
          case None =>
            List(upSlitMode(None), upOffsets(None))

      // Acquisition sub-field updates (each sub-field is independently optional;
      // exposureTimeMode is handled separately via updateExposureTimeModes).
      val acqUpdates: List[AppliedFragment] =
        SET.acquisition.toList.flatMap: acq =>
          val offUpdates: List[AppliedFragment] =
            acq.offset.toList.flatMap: o =>
              List(
                upAcqOffsetP(Some(Angle.microarcseconds.get(o.p.toAngle))),
                upAcqOffsetQ(Some(Angle.microarcseconds.get(o.q.toAngle)))
              )
          List(
            acq.readMode.map(upAcqReadMode),
            acq.coadds.map(upAcqCoadds),
            acq.filter.map(upAcqFilter)
          ).flatten ++ offUpdates

      val ups: List[AppliedFragment] = List(
        SET.coadds.map(c => upCoadds(Some(c))),
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
      ).flatten ++ upTelescope.toList.flatten ++ acqUpdates

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

    def cloneGnirs(
      originalId: Observation.Id,
      newId:      Observation.Id,
      etms:       List[(ExposureTimeModeId, ExposureTimeModeId)]
    ): AppliedFragment =
      val mappingValues: AppliedFragment =
        etms
          .map: (oldEtm, newEtm) =>
            sql"($exposure_time_mode_id, $exposure_time_mode_id)"(oldEtm, newEtm)
          .intercalate(void", ")

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
          c_science_exposure_time_mode_id,
          c_acquisition_exposure_time_mode_id
        )
        SELECT
          $observation_id,
          (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id),
          src.c_observing_mode_type,
          src.c_grating, src.c_prism, src.c_grating_wavelength,
          src.c_initial_grating, src.c_initial_prism,
          src.c_camera, src.c_initial_camera,
          src.c_fpu, src.c_central_wavelength, src.c_initial_fpu, src.c_initial_central_wavelength,
          src.c_filter, src.c_initial_filter,
          src.c_coadds, src.c_decker, src.c_focus_motor_steps, src.c_read_mode, src.c_well_depth,
          src.c_slit_offset_mode, src.c_telescope_configs,
          src.c_acq_read_mode, src.c_acq_coadds, src.c_acq_filter,
          src.c_acq_offset_p, src.c_acq_offset_q,
          sci_map.new_id,
          acq_map.new_id
        FROM t_gnirs_long_slit src
        JOIN (VALUES"""(newId, newId) |+| mappingValues |+| void""") AS sci_map(old_id, new_id)
          ON sci_map.old_id = src.c_science_exposure_time_mode_id
        JOIN (VALUES""" |+| mappingValues |+| sql""") AS acq_map(old_id, new_id)
          ON acq_map.old_id = src.c_acquisition_exposure_time_mode_id
        WHERE src.c_observation_id = $observation_id"""(originalId)
