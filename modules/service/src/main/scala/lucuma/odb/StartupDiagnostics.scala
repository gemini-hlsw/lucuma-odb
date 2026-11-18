// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import cats.data.NonEmptyList
import cats.data.StateT
import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.util.Enumerated
import lucuma.odb.StartupChecks.*
import lucuma.odb.service.ObservationWorkflowService
import lucuma.odb.service.TimeAccounting
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.flamingos_2_acquisition_type
import lucuma.odb.util.GhostCodecs.ghost_ifu_mapping_type
import lucuma.odb.util.GmosCodecs.*
import lucuma.odb.util.GnirsCodecs.*
import org.typelevel.log4cats.Logger
import skunk.Session
import skunk.codec.numeric.int2
import skunk.data.Type
import skunk.syntax.all.*

trait StartupDiagnostics[F[_]]:

  /** Run diagnostics and log failures. If `fatal` is `true` then crash if there are failures. */
  def runAllDiagnostics(fatal: Boolean): F[Unit]

object StartupDiagnostics:

  def apply[F[_]: Concurrent: Logger](db: Session[F]): StartupDiagnostics[F] =

    new StartupDiagnostics[F]:

      given Session[F] = db

      val allDiagnostics: List[Check[F]] =
        List(

          // Postgres Enums
          checkPostgresEnum(arc_type),
          checkPostgresEnum(atom_stage),
          checkPostgresEnum(attachment_type),
          checkPostgresEnum(blind_offset_type),
          checkPostgresEnum(breakpoint),
          checkPostgresEnum(calculation_state),
          checkPostgresEnum(calibration_role),
          checkPostgresEnum(calibration_set_type),
          checkPostgresEnum(calibration_work_type),
          checkPostgresEnum(catalog_name),
          checkPostgresEnum(charge_class),
          checkPostgresEnum(configuration_request_status),
          checkPostgresEnum(consider_for_band_3),
          checkPostgresEnum(dataset_qa_state),
          checkPostgresEnum(dataset_stage),
          checkPostgresEnum(educational_status),
          checkPostgresEnum(email_status),
          checkPostgresEnum(ephemeris_key_type),
          checkPostgresEnum(exchange_partner),
          checkPostgresEnum(execution_event_type),
          checkPostgresEnum(scheduling_mode),
          checkPostgresEnum(execution_state),
          checkPostgresEnum(existence),
          checkPostgresEnum(exposure_time_mode_role),
          checkPostgresEnum(exposure_time_mode_type),
          checkPostgresEnum(flamingos_2_acquisition_type),
          checkPostgresEnum(gcal_baseline),
          checkPostgresEnum(gcal_lamp_type),
          checkPostgresEnum(gemini_proposal_type),
          checkPostgresEnum(ghost_ifu_mapping_type),
          checkPostgresEnum(gnirs_acquisition_type),
          checkPostgresEnum(gnirs_decker),
          checkPostgresEnum(gnirs_fpu_ifu),
          checkPostgresEnum(gnirs_fpu_other),
          checkPostgresEnum(gnirs_well_depth),
          {
            // We'll skip this one.  There is a postgres enum, but it is tied
            // to a scala NewBoolean type.  There is no Enumerated instance.
            StateT.modify[F, DiagState]: ds =>
              ds.updated(Type("e_ghost_fiber_agitator"))
          },
          checkPostgresEnum(imaging_variant),
          checkPostgresEnum(gmos_ifu_acquisition_roi),
          checkPostgresEnum(gmos_long_slit_acquisition_roi),
          checkPostgresEnum(gmos_mos_acquisition_type),
          checkPostgresEnum(gender),
          checkPostgresEnum(archive_duplication_state),
          checkPostgresEnum(group_constraint_violation_code),
          checkPostgresEnum(group_standing_status),
          checkPostgresEnum(guide_state),
          checkPostgresEnum(keck_instrument),
          checkPostgresEnum(multiple_filters_mode),
          checkPostgresEnum(obs_class),
          checkPostgresEnum(observation_priority),
          checkPostgresEnum(observation_qa_state),
          checkPostgresEnum(observation_workflow_state),
          checkPostgresEnum(observatory),
          checkPostgresEnum(observing_mode_row_version),
          checkPostgresEnum(observing_mode_type),
          checkPostgresEnum(offset_generator_role),
          checkPostgresEnum(telescope_config_generator_type),
          checkPostgresEnum(pac_mode),
          checkPostgresEnum(partner_link_type),
          checkPostgresEnum(program_type),
          checkPostgresEnum(program_user_role),
          checkPostgresEnum(science_band),
          checkPostgresEnum(science_subtype),
          checkPostgresEnum(sequence_command),
          checkPostgresEnum(sequence_type),
          checkPostgresEnum(site),
          checkPostgresEnum(slew_stage),
          checkPostgresEnum(smart_gcal_type),
          checkPostgresEnum(step_stage),
          checkPostgresEnum(step_type),
          checkPostgresEnum(subaru_instrument),
          checkPostgresEnum(subaru_proposal_type),
          checkPostgresEnum(summary_generation_state),
          checkPostgresEnum(summary_style),
          checkPostgresEnum(tac_partner_decision),
          checkPostgresEnum(target_disposition),
          checkPostgresEnum(telluric_slot),
          checkPostgresEnum(tg_op),
          checkPostgresEnum(time_charge_correction_op),
          checkPostgresEnum(time_charge_discount_type),
          checkPostgresEnum(timing_window_end_type),
          checkPostgresEnum(timing_window_inclusion),
          checkPostgresEnum(too_activation),
          checkPostgresEnum(too_trigger_status),
          checkPostgresEnum(user_invitation_status),
          {
            // Hack, sorry. ObservationWorkflowService.UserState is an enum in the db but a union type here.
            NonEmptyList
              .fromList:
                Enumerated[ObservationWorkflowState].all.collect:
                  case a: ObservationWorkflowService.UserState => a
              .fold(StateT.pure(())): nel =>
                given Enumerated[ObservationWorkflowService.UserState] =
                  Enumerated.fromNEL(nel).withTag(_.tag)
                checkPostgresEnum(user_state)
          },
          checkPostgresEnum(user_type),
          checkPostgresEnum(visit_origin),
          checkPostgresEnum(wavelength_order),

          // Postgres Lookup Tables
          checkPostgresLookupTable(atom_execution_state, "t_atom_execution_state"),
          checkPostgresLookupTable(cloud_extinction_preset, "t_cloud_extinction"),
          checkPostgresLookupTable(conditions_expectation_type, "t_conditions_expectation_type"),
          checkPostgresLookupTable(conditions_measurement_source, "t_conditions_source"),
          checkPostgresLookupTable(filterType, "t_filter_type"),
          checkPostgresLookupTable(focal_plane, "t_focal_plane"),
          checkPostgresLookupTable(gcal_continuum, "t_gcal_continuum"),
          checkPostgresLookupTable(gcal_diffuser, "t_gcal_diffuser"),
          checkPostgresLookupTable(gcal_filter, "t_gcal_filter"),
          checkPostgresLookupTable(gcal_shutter, "t_gcal_shutter"),
          checkPostgresLookupTable(gmos_amp_count, "t_gmos_amp_count"),
          checkPostgresLookupTable(gmos_amp_gain, "t_gmos_amp_gain"),
          checkPostgresLookupTable(gmos_amp_read_mode, "t_gmos_amp_read_mode"),
          checkPostgresLookupTable(gmos_binning, "t_gmos_binning"),
          checkPostgresLookupTable(gmos_custom_slit_width, "t_gmos_custom_slit_width"),
          checkPostgresLookupTable(gmos_dtax, "t_gmos_dtax"),
          checkPostgresLookupTable(gmos_grating_order, "t_gmos_disperser_order"),
          checkPostgresLookupTable(gmos_north_detector, "t_gmos_north_detector"),
          checkPostgresLookupTable(gmos_north_filter, "t_gmos_north_filter"),
          checkPostgresLookupTable(gmos_north_fpu, "t_gmos_north_fpu"),
          checkPostgresLookupTable(gmos_north_grating, "t_gmos_north_disperser"),
          // Both IFU FPU enums are backed by the one shared table, so checking
          // each also catches the two drifting apart.
          checkPostgresLookupTable(gmos_north_ifu_fpu, "t_gmos_ifu_fpu"),
          checkPostgresLookupTable(gmos_south_ifu_fpu, "t_gmos_ifu_fpu"),
          checkPostgresLookupTable(gmos_north_stage_mode, "t_gmos_north_stage_mode"),
          checkPostgresLookupTable(gmos_roi, "t_gmos_roi"),
          checkPostgresLookupTable(gmos_south_detector, "t_gmos_south_detector"),
          checkPostgresLookupTable(gmos_south_filter, "t_gmos_south_filter"),
          checkPostgresLookupTable(gmos_south_fpu, "t_gmos_south_fpu"),
          checkPostgresLookupTable(gmos_south_grating, "t_gmos_south_disperser"),
          checkPostgresLookupTable(gmos_south_stage_mode, "t_gmos_south_stage_mode"),
          checkPostgresLookupTable(gnirs_camera, "t_gnirs_camera"),
          checkPostgresLookupTable(gnirs_filter, "t_gnirs_filter"),
          checkPostgresLookupTable(gnirs_fpu_slit, "t_gnirs_fpu_slit"),
          checkPostgresLookupTable(gnirs_grating, "t_gnirs_grating"),
          checkPostgresLookupTable(gnirs_prism, "t_gnirs_prism"),
          checkPostgresLookupTable(guide_probe, "t_guide_probe"),
          checkPostgresEnum(altair_mode),
          checkPostgresEnum(altair_nd_filter),
          checkPostgresEnum(cass_rotator),
          checkPostgresEnum(field_lens),
          checkPostgresLookupTable(slit_offset_mode, "t_slit_offset_mode"),
          checkPostgresLookupTable(image_quality_preset, "t_image_quality"),
          checkPostgresLookupTable(imaging_capability, "t_imaging_capability"),
          checkPostgresLookupTable(instrument, "t_instrument"),
          checkPostgresLookupTable(program_status, "t_program_status"),
          checkPostgresLookupTable(proposal_status, "t_proposal_status"),
          checkPostgresLookupTable(seeing_trend, "t_seeing_trend"),
          checkPostgresLookupTable(spectroscopy_capability, "t_spectroscopy_capability"),
          checkPostgresLookupTable(step_execution_state, "t_step_execution_state"),
          checkPostgresLookupTable(water_vapor, "t_water_vapor"),

          // Enumerated types that are used in the database schema but don't exist in the model
          assertUnusedPostgresEnum(Type("e_target_type")),
          assertUnusedPostgresEnum(Type("e_source_profile_type")),
          assertUnusedPostgresEnum(Type("e_summary_job_state")),

          // The time accounting rules
          checkTimeAccountingVersion,

          // This should come last
          checkEnumCoverage,

        )

      // The rules that the service implements must be the latest ones that the
      // database knows about, or else visits would be computed under one
      // version of the rules and recorded as another.
      def checkTimeAccountingVersion: Check[F] =
        StateT.modifyF: ds =>
          db.unique(sql"SELECT current_time_accounting_version()".query(int2.opt)).map:
            case Some(v) if v === TimeAccounting.version.Current =>
              ds
            case Some(v) =>
              ds.updated(List(
                s"Time accounting: the service implements rules version ${TimeAccounting.version.Current} but the latest version in t_time_accounting_version is $v."
              ))
            case None    =>
              ds.updated(List(
                s"Time accounting: t_time_accounting_version is empty, but the service implements rules version ${TimeAccounting.version.Current}."
              ))

      def runAllDiagnostics(fatal: Boolean): F[Unit] =
        runChecks(allDiagnostics, fatal)
