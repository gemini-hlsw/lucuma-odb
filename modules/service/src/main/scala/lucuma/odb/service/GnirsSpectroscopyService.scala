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
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStep
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.graphql.input.GnirsSpectroscopyInput
import lucuma.odb.sequence.gnirs.spectroscopy.AcquisitionConfig
import lucuma.odb.sequence.gnirs.spectroscopy.Config
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GnirsCodecs.*
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import Services.Syntax.*

trait GnirsSpectroscopyService[F[_]]:
  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config]]

  def insert(
    input:  GnirsSpectroscopyInput.Create,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def update(
    SET:   GnirsSpectroscopyInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def clone(originalId: Observation.Id, newId: Observation.Id)(using Transaction[F]): F[Unit]

  /** Reset `oid`'s configuration to telluric defaults (config-dependent slit offsets). */
  def resetTelluricConfig(oid: Observation.Id)(using Transaction[F]): F[Unit]

object GnirsSpectroscopyService:

  def instantiate[F[_]: {Concurrent as F, Services}]: GnirsSpectroscopyService[F] =

    new GnirsSpectroscopyService[F]:

      val gnirsLS: Decoder[Config] = (
        // Science ETM (joined from t_exposure_time_mode via FK)
        exposure_time_mode               *:
        // Effective grating/prism/wavelength for the acquisition configuration
        gnirs_grating                    *: // c_grating_effective
        gnirs_prism                      *: // c_prism_effective
        wavelength_pm                    *: // c_central_wavelength_effective
        // Camera
        gnirs_camera                     *:
        // FPU (exactly one of slit / ifu)
        gnirs_fpu_spectroscopy           *:
        // Filter
        gnirs_filter                     *:
        // Coadds
        int4                             *:
        // Decker effective (DB-computed COALESCE)
        gnirs_decker                     *: // c_decker_effective
        // Read mode explicit override (None => compute from exposure time)
        gnirs_read_mode.opt              *: // c_read_mode (explicit)
        // Well depth effective (DB-computed COALESCE)
        gnirs_well_depth                 *: // c_well_depth_effective
        // Focus motor steps (nullable = Best)
        int4.opt                         *:
        // Telescope configs effective: slit offset mode (NULL for IFU) + JSON
        slit_offset_mode.opt             *: // c_slit_offset_mode_effective
        text                             *: // c_telescope_configs_effective
        // Acquisition fields (inline cols + acq ETM joined from t_exposure_time_mode via FK)
        gnirs_acquisition_type.opt       *: // c_acq_type (None => AUTO mode)
        int4                             *: // c_acq_coadds
        gnirs_filter.opt                 *: // c_acq_filter (explicit override; None => mode default)
        angle_µas.opt                    *: // c_acq_sky_offset_p
        angle_µas.opt                    *: // c_acq_sky_offset_q
        exposure_time_mode               *: // acquisition ETM
        telluric_type                       // c_telluric_type
      ).emap:
        case (sciEtm *: gratingEff *: prismEff *: centralWavEff *:
              camera *: fpu *: filter *: coadds *:
              deckerEff *:
              readModeExp *:
              wellDepthEff *:
              focusMotorSteps *:
              slitOffsetModeEff *: tcEff *:
              acqType *: acqCoadds *: acqFilterExp *: acqSkyOffP *: acqSkyOffQ *:
              acqEtm *: telluricType *: EmptyTuple) =>
          // IFU (slit offset mode NULL) carries a plain [TelescopeConfig]; long slit resolves
          // its SlitTelescopeConfigs to the same.
          val telescopeConfigs: Either[String, NonEmptyList[TelescopeConfig]] =
            slitOffsetModeEff match
              case Some(mode) =>
                SlitTelescopeConfigsFormat.getOption((mode, tcEff)).map(_.telescopeConfigs)
                  .toRight(s"Could not parse telescope configs from '$tcEff'")
              case None       =>
                ToSkyFormat.getOption(tcEff)
                  .toRight(s"Could not parse IFU telescope configs from '$tcEff'")
          telescopeConfigs.flatMap { resolvedTC =>
                PosInt.from(coadds)
                  .leftMap(e => s"Invalid coadds $coadds: $e")
                  .flatMap: coaddsP =>
                    PosInt.from(acqCoadds)
                      .leftMap(e => s"Invalid acq coadds $acqCoadds: $e")
                      .map: acqCoaddsP =>
                        // Explicit acquisition mode (None => AUTO); the sky offset is
                        // present only for an explicit FAINT type (DB CHECK enforced) and
                        // is carried inside the Faint mode. The filter override is separate.
                        val acqSkyOffset: Option[Offset] =
                          (acqSkyOffP, acqSkyOffQ).mapN((p, q) => Offset(Offset.P(p), Offset.Q(q)))
                        val explicitAcqMode: Option[GnirsAcquisitionMode] =
                          acqType.map(GnirsAcquisitionMode.forTypeAndOffset(_, acqSkyOffset))
                        val acq = AcquisitionConfig(explicitAcqMode, acqFilterExp, acqEtm, acqCoaddsP)
                        val focus = focusMotorSteps.fold(GnirsFocus.Best): n =>
                          GnirsFocus.Custom(GnirsFocusMotorStepsValue.unsafeFrom(n).withUnit[GnirsFocusMotorStep])
                        Config(
                          filter,
                          deckerEff,
                          fpu,
                          prismEff,
                          gratingEff,
                          centralWavEff,
                          camera,
                          focus,
                          readModeExp,
                          wellDepthEff,
                          sciEtm,
                          coaddsP,
                          resolvedTC,
                          acq,
                          telluricType
                          )
              }

      override def select(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        NonEmptyList
          .fromList(which)
          .fold(Map.empty.pure[F]): oids =>
            val af = Statements.selectGnirsSpectroscopy(oids)
            session.prepareR(af.fragment.query(observation_id *: gnirsLS)).use: pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList.map(_.toMap)

      private def insertExposureTimeModes(
        input: GnirsSpectroscopyInput.Create,
        req:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        val acqEtm: Option[ExposureTimeMode] = input.acquisition.flatMap(_.exposureTimeMode)
        exposureTimeModeService
          .insertOneWithDefaults("GNIRS Spectroscopy", acqEtm, input.exposureTimeMode, req, which)
          .map(_.void)

      override def insert(
        input:  GnirsSpectroscopyInput.Create,
        req:    Option[ExposureTimeMode],
        which:  List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        (for
          _ <- ResultT(insertExposureTimeModes(input, req, which))
          _ <- ResultT.liftF:
            which.traverse: oid =>
              session.exec(Statements.insertGnirsSpectroscopy(oid, input))
            .void
        yield ()).value

      override def delete(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.deleteGnirs(which).fold(F.unit)(session.exec)

      private def updateExposureTimeModes(
        input: GnirsSpectroscopyInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        def update(etm: Option[ExposureTimeMode], role: ExposureTimeModeRole): F[Unit] =
          etm.fold(().pure[F]): e =>
            services.exposureTimeModeService.updateMany(which, role, e)
        for
          _ <- update(input.acquisition.flatMap(_.exposureTimeMode), ExposureTimeModeRole.Acquisition)
          _ <- update(input.exposureTimeMode, ExposureTimeModeRole.Science)
        yield ()

      // A NonNull telescope-config override of a specific kind must match the persisted FPU.
      // The input validates this against an edited FPU; when the FPU is unchanged we check the
      // persisted observing-mode type here (the DB CHECK is the final backstop).
      private def validateTelescopeConfigKind(
        SET:   GnirsSpectroscopyInput.Edit,
        which: List[Observation.Id]
      ): F[Result[Unit]] =
        val slitOverride = SET.explicitTelescopeConfigsSlit.isPresent
        val ifuOverride  = SET.explicitTelescopeConfigsIfu.isPresent
        if SET.fpu.isDefined || !(slitOverride || ifuOverride) then Result.unit.pure[F]
        else
          NonEmptyList.fromList(which).fold(Result.unit.pure[F]): oids =>
            val af = Statements.selectObservingModeTypes(oids)
            session.prepareR(af.fragment.query(observing_mode_type)).use: pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList
            .map: types =>
              if slitOverride && types.exists(_ =!= ObservingModeType.GnirsLongSlit) then
                OdbError.InvalidArgument("'explicitTelescopeConfigsSlit' is only valid with a long-slit FPU.".some).asFailure
              else if ifuOverride && types.exists(_ =!= ObservingModeType.GnirsIfu) then
                OdbError.InvalidArgument("'explicitTelescopeConfigsIfu' is only valid with an IFU FPU.".some).asFailure
              else Result.unit

      override def update(
        SET:   GnirsSpectroscopyInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        (for
          _ <- ResultT(validateTelescopeConfigKind(SET, which))
          _ <- ResultT.liftF(updateExposureTimeModes(SET, which))
          _ <- ResultT.liftF(Statements.updateGnirsSpectroscopy(SET, which).fold(F.unit)(session.exec))
        yield ()).value

      override def clone(originalId: Observation.Id, newId: Observation.Id)(using Transaction[F]): F[Unit] =
        session.exec(Statements.cloneGnirs(originalId, newId))

      override def resetTelluricConfig(oid: Observation.Id)(using Transaction[F]): F[Unit] =
        session.exec(Statements.applyGnirsTelluricDefaults(oid))

  object Statements:

    def selectGnirsSpectroscopy(observationIds: NonEmptyList[Observation.Id]): AppliedFragment =
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
          ls.c_central_wavelength_effective,
          ls.c_camera,
          ls.c_fpu_slit,
          ls.c_fpu_ifu,
          ls.c_filter,
          ls.c_coadds,
          ls.c_decker_effective,
          ls.c_read_mode,
          ls.c_well_depth_effective,
          ls.c_focus_motor_steps,
          ls.c_slit_offset_mode_effective,
          ls.c_telescope_configs_effective,
          ls.c_acq_type,
          ls.c_acq_coadds,
          ls.c_acq_filter,
          ls.c_acq_sky_offset_p,
          ls.c_acq_sky_offset_q,
          acq.c_exposure_time_mode,
          acq.c_signal_to_noise_at,
          acq.c_signal_to_noise,
          acq.c_exposure_time,
          acq.c_exposure_count,
          ls.c_telluric_type
        FROM v_gnirs_spectroscopy ls
        LEFT JOIN t_exposure_time_mode sci
           ON sci.c_observation_id = ls.c_observation_id
          AND sci.c_role = 'science'
        LEFT JOIN t_exposure_time_mode acq
           ON acq.c_observation_id = ls.c_observation_id
          AND acq.c_role = 'acquisition'
      """(Void) |+|
      void"WHERE ls.c_observation_id IN (" |+|
        observationIds.map(sql"$observation_id").intercalate(void",") |+|
      void")"

    // None => no explicit acquisition type; resolved from the exposure time at
    // sequence-generation time (mirrors read mode handling).
    private def explicitAcqType(input: GnirsSpectroscopyInput.Create): Option[GnirsAcquisitionType] =
      input.acquisition.flatMap(_.explicitAcqType.toOption)

    val InsertGnirsSpectroscopy: Fragment[(
      Observation.Id,
      // observing mode type (gnirs_long_slit for slit, gnirs_ifu for ifu)
      ObservingModeType,
      // initial mirror
      GnirsGrating,
      GnirsPrism,
      // camera/fpu/filter
      GnirsCamera,
      GnirsFpu.Spectroscopy,
      GnirsFilter,
      // coadds
      Int,
      // central wavelength (required; stored as the initial value, override left NULL)
      Wavelength,
      // explicit overrides (all nullable)
      Option[GnirsDecker],
      Option[GnirsGrating],   // explicit grating
      Option[GnirsPrism],     // explicit prism
      Option[Int],            // focus_motor_steps
      Option[GnirsReadMode],
      Option[GnirsWellDepth],
      // telescope configs (nullable = use default)
      Option[SlitOffsetMode],
      Option[String],
      // acquisition inline columns
      Option[GnirsAcquisitionType], // None => automatic
      Int,
      Option[GnirsFilter],    // explicit acq filter (None => computed default)
      Option[Long],           // acq_sky_offset_p in µas
      Option[Long],           // acq_sky_offset_q in µas
      TelluricType            // telluric type
    )] =
      sql"""
        INSERT INTO t_gnirs_spectroscopy (
          c_observation_id,
          c_program_id,
          c_observing_mode_type,
          c_initial_grating,
          c_initial_prism,
          c_camera,
          c_initial_camera,
          c_fpu_slit,
          c_fpu_ifu,
          c_initial_fpu_slit,
          c_initial_fpu_ifu,
          c_filter,
          c_initial_filter,
          c_coadds,
          c_decker,
          c_central_wavelength,
          c_initial_central_wavelength,
          c_grating,
          c_prism,
          c_focus_motor_steps,
          c_read_mode,
          c_well_depth,
          c_slit_offset_mode,
          c_telescope_configs,
          c_acq_type,
          c_acq_coadds,
          c_acq_filter,
          c_acq_sky_offset_p,
          c_acq_sky_offset_q,
          c_telluric_type
        )
        SELECT
          $observation_id,
          c_program_id,
          $observing_mode_type,
          $gnirs_grating,
          $gnirs_prism,
          $gnirs_camera,
          $gnirs_camera,
          $gnirs_fpu_spectroscopy,
          $gnirs_fpu_spectroscopy,
          $gnirs_filter,
          $gnirs_filter,
          $int4,
          ${gnirs_decker.opt},
          NULL,
          $wavelength_pm,
          ${gnirs_grating.opt},
          ${gnirs_prism.opt},
          ${int4.opt},
          ${gnirs_read_mode.opt},
          ${gnirs_well_depth.opt},
          ${slit_offset_mode.opt},
          ${text.opt},
          ${gnirs_acquisition_type.opt},
          $int4,
          ${gnirs_filter.opt},
          ${int8.opt},
          ${int8.opt},
          $telluric_type
        FROM t_observation
        WHERE c_observation_id = $observation_id
      """.contramap {
        (oid, obsModeType, initGrating, initPrism, camera, fpu, filter, coadds,
         centralWav, decker, explGrating, explPrism, focus,
         readMode, wellDepth, slitMode, offsets,
         acqType, acqCoadds, acqFilter, acqSkyOffP, acqSkyOffQ, telluricType) =>
          // fpu appears twice (current + initial); the spectroscopy codec expands each
          // into the (c_fpu_slit, c_fpu_ifu) column pair.
          (oid, obsModeType, initGrating, initPrism, camera, camera, fpu, fpu,
           filter, filter, coadds, decker, centralWav,
           explGrating, explPrism, focus, readMode, wellDepth,
           slitMode, offsets,
           acqType, acqCoadds, acqFilter, acqSkyOffP, acqSkyOffQ, telluricType, oid)
      }

    def insertGnirsSpectroscopy(
      observationId: Observation.Id,
      input:         GnirsSpectroscopyInput.Create
    ): AppliedFragment =
      // Slit configs persist (offset mode, JSON); IFU configs persist as JSON with a NULL mode.
      val explicitSlitTC = input.explicitTelescopeConfigsSlit.map(SlitTelescopeConfigsFormat.reverseGet)
      val explicitSlitMode = explicitSlitTC.map(_._1)
      val explicitTcJson   = explicitSlitTC.map(_._2).orElse(input.explicitTelescopeConfigsIfu.map(ToSkyFormat.reverseGet))
      val acqSkyOffP = input.acquisition.flatMap(_.skyOffset).map(o => Angle.microarcseconds.get(o.p.toAngle))
      val acqSkyOffQ = input.acquisition.flatMap(_.skyOffset).map(o => Angle.microarcseconds.get(o.q.toAngle))
      InsertGnirsSpectroscopy.apply(
        observationId,
        input.observingModeType,
        input.grating,
        input.prism,
        input.camera,
        input.fpu,
        input.filter,
        input.coadds.map(_.value).getOrElse(1),
        input.centralWavelength,
        input.explicitDecker,
        input.explicitGrating,
        input.explicitPrism,
        input.explicitFocusMotorSteps,
        input.explicitReadMode,
        input.explicitWellDepth,
        explicitSlitMode,
        explicitTcJson,
        explicitAcqType(input),
        input.acquisition.flatMap(_.coadds).map(_.value).getOrElse(1),
        input.acquisition.flatMap(_.explicitFilter.toOption),
        acqSkyOffP,
        acqSkyOffQ,
        input.telluricType
      )

    def selectObservingModeTypes(oids: NonEmptyList[Observation.Id]): AppliedFragment =
      void"SELECT DISTINCT c_observing_mode_type FROM t_gnirs_spectroscopy WHERE " |+| observationIdIn(oids)

    def deleteGnirs(which: List[Observation.Id]): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map: oids =>
        void"DELETE FROM ONLY t_gnirs_spectroscopy " |+|
          void"WHERE " |+| observationIdIn(oids)

    private def gnirsUpdates(SET: GnirsSpectroscopyInput.Edit): Option[NonEmptyList[AppliedFragment]] =
      val upCoadds       = sql"c_coadds             = ${int4_pos.opt}"
      val upFilter       = sql"c_filter             = ${gnirs_filter.opt}"
      // FPU edits set the matching column only. Switching kind (slit<->ifu) would also
      // require changing the observation's mode type, so it is not supported here: a
      // cross-kind edit leaves both columns populated and fails the exactly-one CHECK.
      def fpuUpdates(fpu: GnirsFpu.Spectroscopy): AppliedFragment =
        fpu match
          case GnirsFpu.Spectroscopy.Slit(s) => sql"c_fpu_slit = $gnirs_fpu_slit".apply(s)
          case GnirsFpu.Spectroscopy.Ifu(i)  => sql"c_fpu_ifu  = $gnirs_fpu_ifu".apply(i)
      val upCamera       = sql"c_camera             = ${gnirs_camera.opt}"
      val upDecker       = sql"c_decker             = ${gnirs_decker.opt}"
      val upCentralWav   = sql"c_central_wavelength = $wavelength_pm"
      val upFocus        = sql"c_focus_motor_steps  = ${int4.opt}"
      val upReadMode     = sql"c_read_mode          = ${gnirs_read_mode.opt}"
      val upWellDepth    = sql"c_well_depth         = ${gnirs_well_depth.opt}"
      val upGrating      = sql"c_grating            = ${gnirs_grating.opt}"
      val upPrism        = sql"c_prism              = ${gnirs_prism.opt}"
      val upSlitMode     = sql"c_slit_offset_mode   = ${slit_offset_mode.opt}"
      val upOffsets      = sql"c_telescope_configs  = ${text.opt}"
      val upTelluricType = sql"c_telluric_type      = $telluric_type"

      // Acquisition inline (non-ETM) column updates
      val upAcqType      = sql"c_acq_type = ${gnirs_acquisition_type.opt}"
      val upAcqCoadds    = sql"c_acq_coadds    = $int4_pos"
      val upAcqFilter    = sql"c_acq_filter    = ${gnirs_filter.opt}"
      val upAcqSkyOffP   = sql"c_acq_sky_offset_p  = ${int8.opt}"
      val upAcqSkyOffQ   = sql"c_acq_sky_offset_q  = ${int8.opt}"

      // Slit and IFU explicit configs are mutually exclusive (input-validated) and share
      // the (c_slit_offset_mode, c_telescope_configs) columns. IFU writes a NULL mode.
      val upTelescope: Option[List[AppliedFragment]] =
        SET.explicitTelescopeConfigsSlit.toOptionOption.map:
          case Some(tc) =>
            val (mode, off) = SlitTelescopeConfigsFormat.reverseGet(tc)
            List(upSlitMode(Some(mode)), upOffsets(Some(off)))
          case None =>
            List(upSlitMode(None), upOffsets(None))
        .orElse:
          SET.explicitTelescopeConfigsIfu.toOptionOption.map:
            case Some(cs) => List(upSlitMode(None), upOffsets(Some(ToSkyFormat.reverseGet(cs))))
            case None     => List(upSlitMode(None), upOffsets(None))

      // Acquisition sub-field updates (exposureTimeMode is handled separately via
      // updateExposureTimeModes). The acquisition type and sky offset are coupled:
      // input validation guarantees a sky offset is present iff the explicit type is
      // FAINT, so whenever the type is (re)set we rewrite the offset columns too —
      // the provided offset for FAINT, NULL otherwise. When the type is left
      // unchanged we touch neither (and no orphan offset can be supplied).
      val acqUpdates: List[AppliedFragment] =
        SET.acquisition.toList.flatMap: acq =>
          val typeAndOffset: List[AppliedFragment] =
            acq.explicitAcqType.toOptionOption match
              case Some(tOpt) =>
                List(
                  upAcqType(tOpt),
                  upAcqSkyOffP(acq.skyOffset.map(o => Angle.microarcseconds.get(o.p.toAngle))),
                  upAcqSkyOffQ(acq.skyOffset.map(o => Angle.microarcseconds.get(o.q.toAngle)))
                )
              case None       =>
                Nil
          List(
            acq.coadds.map(upAcqCoadds),
            acq.explicitFilter.toOptionOption.map(upAcqFilter)
          ).flatten ++ typeAndOffset

      val ups: List[AppliedFragment] = List(
        SET.coadds.map(c => upCoadds(Some(c))),
        SET.filter.map(f => upFilter(Some(f))),
        SET.fpu.map(fpuUpdates),
        SET.camera.map(c => upCamera(Some(c))),
        SET.explicitDecker.toOptionOption.map(upDecker),
        SET.centralWavelength.map(upCentralWav),
        SET.explicitFocusMotorSteps.toOptionOption.map(upFocus),
        SET.explicitReadMode.toOptionOption.map(upReadMode),
        SET.explicitWellDepth.toOptionOption.map(upWellDepth),
        SET.explicitGrating.toOptionOption.map(upGrating),
        SET.explicitPrism.toOptionOption.map(upPrism),
        SET.telluricType.map(upTelluricType)
      ).flatten ++ upTelescope.toList.flatten ++ acqUpdates

      NonEmptyList.fromList(ups)

    def updateGnirsSpectroscopy(
      SET: GnirsSpectroscopyInput.Edit,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      for
        us   <- gnirsUpdates(SET)
        oids <- NonEmptyList.fromList(which)
      yield
        void"UPDATE t_gnirs_spectroscopy " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    // Reset `oid` to telluric slit-offset defaults. The telluric offsets parallel the
    // science offsets computed by v_gnirs_spectroscopy (V1165/V1169), per the GNIRS
    // configuration (cross-dispersed / short camera / long camera) and grating
    // wavelength regime (filters Order2/Order1/PAH are >= 2.5 µm):
    //   Cross-dispersed prisms        → [+1", -2", -2", +1"]
    //   Short camera long slit        → [-2", +4", +4", -2"]
    //   Long camera, filter >= 2.5 µm → [-3", +3", +3", -3"]
    //   Long camera, filter < 2.5 µm  → [+1", -5", -5", +1"]
    def applyGnirsTelluricDefaults(oid: Observation.Id): AppliedFragment =
      sql"""
        UPDATE t_gnirs_spectroscopy
        SET
          c_slit_offset_mode  = 'nod_along_slit',
          c_telescope_configs = CASE
            WHEN COALESCE(c_prism, c_initial_prism) IN ('Sxd', 'Lxd') THEN
              '[{"q":{"microarcseconds":1000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-2000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-2000000},"guiding":"ENABLED"},{"q":{"microarcseconds":1000000},"guiding":"ENABLED"}]'
            WHEN c_camera IN ('ShortBlue', 'ShortRed') THEN
              '[{"q":{"microarcseconds":-2000000},"guiding":"ENABLED"},{"q":{"microarcseconds":4000000},"guiding":"ENABLED"},{"q":{"microarcseconds":4000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-2000000},"guiding":"ENABLED"}]'
            WHEN c_filter IN ('Order2', 'Order1', 'PAH') THEN
              '[{"q":{"microarcseconds":-3000000},"guiding":"ENABLED"},{"q":{"microarcseconds":3000000},"guiding":"ENABLED"},{"q":{"microarcseconds":3000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-3000000},"guiding":"ENABLED"}]'
            ELSE
              '[{"q":{"microarcseconds":1000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-5000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-5000000},"guiding":"ENABLED"},{"q":{"microarcseconds":1000000},"guiding":"ENABLED"}]'
          END
        WHERE c_observation_id = $observation_id
      """.apply(oid)

    def cloneGnirs(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      sql"""
        INSERT INTO t_gnirs_spectroscopy (
          c_observation_id, c_program_id, c_observing_mode_type,
          c_grating, c_prism, c_central_wavelength, c_initial_central_wavelength,
          c_initial_grating, c_initial_prism,
          c_camera, c_initial_camera,
          c_fpu_slit, c_fpu_ifu, c_initial_fpu_slit, c_initial_fpu_ifu,
          c_filter, c_initial_filter,
          c_coadds, c_decker, c_focus_motor_steps, c_read_mode, c_well_depth,
          c_slit_offset_mode, c_telescope_configs,
          c_acq_type, c_acq_coadds, c_acq_filter,
          c_acq_sky_offset_p, c_acq_sky_offset_q,
          c_telluric_type
        )
        SELECT
          $observation_id,
          (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id),
          c_observing_mode_type,
          c_grating, c_prism, c_central_wavelength, c_initial_central_wavelength,
          c_initial_grating, c_initial_prism,
          c_camera, c_initial_camera,
          c_fpu_slit, c_fpu_ifu, c_initial_fpu_slit, c_initial_fpu_ifu,
          c_filter, c_initial_filter,
          c_coadds, c_decker, c_focus_motor_steps, c_read_mode, c_well_depth,
          c_slit_offset_mode, c_telescope_configs,
          c_acq_type, c_acq_coadds, c_acq_filter,
          c_acq_sky_offset_p, c_acq_sky_offset_q,
          c_telluric_type
        FROM t_gnirs_spectroscopy
        WHERE c_observation_id = $observation_id
      """.apply(newId, newId, originalId)
