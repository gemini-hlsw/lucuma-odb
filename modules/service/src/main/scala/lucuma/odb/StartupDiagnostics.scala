// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import cats.data.StateT
import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.core.util.Enumerated
import lucuma.odb.util.Codecs.*
import org.tpolecat.typename.TypeName
import org.typelevel.log4cats.Logger
import skunk.Codec
import skunk.Session
import skunk.codec.text.varchar
import skunk.data.Type
import skunk.syntax.all.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.odb.service.ObservationWorkflowService
import cats.data.NonEmptyList
import lucuma.odb.util.GmosCodecs.*

trait StartupDiagnostics[F[_]]:

  /** Run diagnostics and log failures. If `fatal` is `true` then crash if there are failures. */
  def runAllDiagnostics(fatal: Boolean): F[Unit]

object StartupDiagnostics:

  case class DiagState(
    types: List[Type],
    errors: List[String]
  ):
    def updated(newTypes: List[Type], newErrors: List[String]): DiagState = DiagState(types ++ newTypes, errors ++ newErrors)
    def updated(newErrors: List[String]): DiagState = DiagState(types, errors ++ newErrors)
    def updated(newType: Type): DiagState = DiagState(newType :: types, errors)
  object DiagState:
    val Initial = DiagState(Nil, Nil)

  def apply[F[_]: Concurrent: Logger](db: Session[F]): StartupDiagnostics[F] =

    new StartupDiagnostics[F]:

      val allDiagnostics: List[StateT[F, DiagState, Unit]] =
        List(

          // Postgres Enums
          checkPostgresEnum(arc_type),
          checkPostgresEnum(atom_stage),
          checkPostgresEnum(attachment_type),
          checkPostgresEnum(blind_offset_type),
          checkPostgresEnum(calculation_state),
          checkPostgresEnum(calibration_role),
          checkPostgresEnum(catalog_name),
          checkPostgresEnum(cfp_type),
          checkPostgresEnum(charge_class),
          checkPostgresEnum(configuration_request_status),
          checkPostgresEnum(dataset_qa_state),
          checkPostgresEnum(dataset_stage),
          checkPostgresEnum(educational_status),
          checkPostgresEnum(email_status),
          checkPostgresEnum(ephemeris_key_type),
          checkPostgresEnum(execution_event_type),
          checkPostgresEnum(execution_state),
          checkPostgresEnum(existence),
          checkPostgresEnum(exposure_time_mode_role),
          checkPostgresEnum(exposure_time_mode_type),
          checkPostgresEnum(gcal_baseline),
          checkPostgresEnum(gcal_lamp_type),
          checkPostgresEnum(gmos_long_slit_acquisition_roi),
          checkPostgresEnum(gender),
          checkPostgresEnum(guide_state),
          checkPostgresEnum(multiple_filters_mode),
          checkPostgresEnum(obs_class),
          checkPostgresEnum(observation_workflow_state),          
          checkPostgresEnum(observing_mode_row_version),
          checkPostgresEnum(observing_mode_type),
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
          checkPostgresEnum(target_disposition),
          checkPostgresEnum(tg_op),
          checkPostgresEnum(time_charge_correction_op),
          checkPostgresEnum(time_charge_discount_type),
          checkPostgresEnum(timing_window_end_type),
          checkPostgresEnum(timing_window_inclusion),
          checkPostgresEnum(too_activation),
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

          // Postgres Lookup Tables
          checkPostgresLookupTable(atom_execution_state, "t_atom_execution_state"),
          checkPostgresLookupTable(cloud_extinction_preset, "t_cloud_extinction"),
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
          // checkPostgresLookupTable(gmos_grating_order, "t_gmos_grating_order"),
          checkPostgresLookupTable(gmos_north_detector, "t_gmos_north_detector"),
          checkPostgresLookupTable(gmos_north_filter, "t_gmos_north_filter"),
          checkPostgresLookupTable(gmos_north_fpu, "t_gmos_north_fpu"),
          // checkPostgresLookupTable(gmos_north_grating, "t_gmos_north_grating"),
          checkPostgresLookupTable(gmos_north_stage_mode, "t_gmos_north_stage_mode"),
          checkPostgresLookupTable(gmos_roi, "t_gmos_roi"),
          checkPostgresLookupTable(gmos_south_detector, "t_gmos_south_detector"),
          checkPostgresLookupTable(gmos_south_filter, "t_gmos_south_filter"),
          checkPostgresLookupTable(gmos_south_fpu, "t_gmos_south_fpu"),
          // checkPostgresLookupTable(gmos_south_grating, "t_gmos_south_grating"),
          checkPostgresLookupTable(gmos_south_stage_mode, "t_gmos_south_stage_mode"),
          checkPostgresLookupTable(image_quality_preset, "t_image_quality"),
          checkPostgresLookupTable(instrument, "t_instrument"),
          checkPostgresLookupTable(spectroscopy_capabilities, "t_spectroscopy_capabilities"),
          checkPostgresLookupTable(step_execution_state, "t_step_execution_state"),
          checkPostgresLookupTable(water_vapor, "t_water_vapor"),

          // Enumerated types that are used in the database schema but don't exist in the model
          assertUnusedPostgresEnum(Type("e_target_type")),
          assertUnusedPostgresEnum(Type("e_source_profile_type")),

          checkEnumCoverage, // This should come last

        )

      def runAllDiagnostics(fatal: Boolean): F[Unit] =        
        Logger[F].info("Running startup diagnostics.") >>
        allDiagnostics.sequence.runS(DiagState.Initial).map(_.errors).flatMap: errors =>
          errors.traverse_(Logger[F].error(_)) >>
            Logger[F].info("Startup diagnostics passed.").whenA(errors.isEmpty) >>
            Logger[F].error("Startup diagnostics failed.").whenA(errors.nonEmpty) >>
            Concurrent[F].raiseError(new Error(s"Startup diagnostics failed. Exiting.")).whenA(fatal && errors.nonEmpty)

      def assertUnusedPostgresEnum(tpe: Type): StateT[F, DiagState, Unit] =
        StateT.modify(_.updated(tpe))

      def checkPostgresEnum[E: Enumerated](decoder: Codec[E]): StateT[F, DiagState, Unit] =
        StateT.modifyF: ds =>
          db.stream(sql"SELECT unnest(enum_range(NULL::#${decoder.types.head.name})::_varchar) as value".query(varchar))(skunk.Void, 104)
            .compile
            .toList
            .map(align(decoder, "the database schema"))
            .map(ds.updated(decoder.types, _))

      def checkPostgresLookupTable[E: Enumerated: TypeName](decoder: Codec[E], tableName: String): StateT[F, DiagState, Unit] =
        StateT.modifyF: ds =>
          db.stream(sql"SELECT c_tag FROM #$tableName".query(varchar))(skunk.Void, 104)
            .compile
            .toList
            .map(align(decoder, s"lookup table $tableName"))
            .map(ds.updated(decoder.types, _))

      def checkEnumCoverage: StateT[F, DiagState, Unit] =
        StateT.modifyF: ds =>
          db.stream(sql"SELECT typname::varchar FROM pg_type WHERE typtype = 'e'".query(varchar))(skunk.Void, 104)
            .compile
            .toList
            .map: es =>
              val seen = ds.types.map(_.name).toSet
              val errors = es.filterNot(seen).map(s => s"Postgres enum $s has not been checked.")
              ds.updated(errors)

      /**
       * In the case of `Site` and maybe other places the Scala side tag isn't the same as the database side tag, and this
       * is mediated through the codec. So what we do is construct the element from its Scala tag and serialize it to get
       * its database tag.
       */
      def databaseTag[A](codec: Codec[A])(tag: String)(using e: Enumerated[A]): String =
        e.fromTag(tag).flatMap(codec.encode(_).headOption.flatten).getOrElse(tag)

      def align[A](codec: Codec[A], where: String)(found: List[String])(using e: Enumerated[A], tn: TypeName[A]): List[String] =
        val expected = e.all.map(e.tag).map(databaseTag(codec)).toSet
        val actual   = found.toSet
        (expected ++ actual)
          .toList
          .sorted
          .flatMap: tag =>
            (expected(tag), actual(tag)) match
              case (true, false)  => List(s"${tn.value}: expected tag \"$tag\" was not found in $where.")
              case (false, true)  => List(s"${tn.value}: unexpected tag \"$tag\" was found in $where.")
              case _ => Nil
