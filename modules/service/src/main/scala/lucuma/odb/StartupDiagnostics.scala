// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import lucuma.core.util.Enumerated
import skunk.Decoder
import skunk.Session
import skunk.syntax.all.*
import cats.effect.Concurrent
import cats.syntax.all.*
import org.tpolecat.typename.TypeName
import lucuma.odb.util.Codecs.*
import cats.kernel.Semigroup
import cats.Applicative
import org.typelevel.log4cats.Logger
import skunk.codec.text.varchar
import scala.annotation.nowarn

trait StartupDiagnostics[F[_]]:

  /** Run diagnostics and log failures. If `fatal` is `true` then crash if there are failures. */
  def runAllDiagnostics(fatal: Boolean): F[Unit]

object StartupDiagnostics:

  def apply[F[_]: Concurrent: Logger](db: Session[F]): StartupDiagnostics[F] =

    new StartupDiagnostics[F]:

      val allDiagnostics: List[F[List[String]]] =
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
          checkPostgresEnum(exposure_time_mode_role),
          checkPostgresEnum(exposure_time_mode_type),
          checkPostgresEnum(gcal_baseline),
          checkPostgresEnum(gcal_lamp_type),
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
          checkPostgresEnum(user_type),

          // Postgres Lookup Tables
          checkPostgresLookupTable(atom_execution_state, "t_atom_execution_state"),
          checkPostgresLookupTable(cloud_extinction_preset, "t_cloud_extinction"),
          checkPostgresLookupTable(focal_plane, "t_focal_plane"),
          checkPostgresLookupTable(gcal_continuum, "t_gcal_continuum"),
          checkPostgresLookupTable(gcal_diffuser, "t_gcal_diffuser"),
          checkPostgresLookupTable(gcal_filter, "t_gcal_filter"),
          checkPostgresLookupTable(gcal_shutter, "t_gcal_shutter"),
          checkPostgresLookupTable(image_quality_preset, "t_image_quality"),
          checkPostgresLookupTable(instrument, "t_instrument"),
          checkPostgresLookupTable(spectroscopy_capabilities, "t_spectroscopy_capabilities"),
          checkPostgresLookupTable(step_execution_state, "t_step_execution_state"),
          checkPostgresLookupTable(water_vapor, "t_water_vapor"),

        )

      def runAllDiagnostics(fatal: Boolean): F[Unit] =        
        Logger[F].info("Running startup diagnostics.") >>
        allDiagnostics
         .combineAll(using Applicative.monoid)
          .flatMap: errors =>
              errors.traverse_(Logger[F].error(_)) >>
              Logger[F].info("Startup diagnostics passed.").whenA(errors.isEmpty) >>
              Logger[F].error("Startup diagnostics failed.").whenA(errors.nonEmpty) >>
              Concurrent[F].raiseError(new Error(s"Startup diagnostics failed. Exiting.")).whenA(fatal && errors.nonEmpty)

      def checkPostgresEnum[E: Enumerated: TypeName](decoder: Decoder[E]): F[List[String]] = 
        db.stream(sql"SELECT unnest(enum_range(NULL::#${decoder.types.head.name})::_varchar) as value".query(varchar))(skunk.Void, 1024)
          .compile
          .toList
          .map(align("the database schema"))

      def checkPostgresLookupTable[E: Enumerated: TypeName](@nowarn("msg=unused") decoder: Decoder[E], tableName: String): F[List[String]] = 
        db.stream(sql"SELECT c_tag FROM #$tableName".query(varchar))(skunk.Void, 1024)
          .compile
          .toList
          .map(align(s"lookup table $tableName"))

      def align[A](where: String)(found: List[String])(using e: Enumerated[A], tn: TypeName[A]): List[String] =
        val expected = e.all.map(e.tag).toSet
        val actual   = found.toSet
        (expected ++ actual)
          .toList
          .sorted
          .flatMap: tag =>
            (expected(tag), actual(tag)) match
              case (true, false)  => List(s"${tn.value}: expected tag \"$tag\" was not found in $where.")
              case (false, true)  => List(s"${tn.value}: unexpected tag \"$tag\" was found in $where.")
              case _ => Nil
