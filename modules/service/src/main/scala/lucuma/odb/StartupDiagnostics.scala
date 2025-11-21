// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import cats.data.NonEmptyList
import cats.data.StateT
import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.EnumType
import grackle.Schema
import grackle.Type as GrackleType
import lucuma.core.enums.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.dimensional.*
import lucuma.core.model.*
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.syntax.string.*
import lucuma.core.util.*
import lucuma.core.util.Enumerated
import lucuma.odb.data.*
import lucuma.odb.graphql.enums.*
import lucuma.odb.service.ObservationWorkflowService
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import org.tpolecat.typename.TypeName
import org.typelevel.log4cats.Logger
import skunk.Codec
import skunk.Session
import skunk.codec.text.varchar
import skunk.data.Type as PgType
import skunk.syntax.all.*

trait StartupDiagnostics[F[_]]:

  /** Run diagnostics and log failures. If `fatal` is `true` then crash if there are failures. */
  def runAllDiagnostics(fatal: Boolean): F[Unit]

object StartupDiagnostics:

  case class DiagState(
    pgTypes: List[PgType],
    grackleTypes: List[GrackleType],
    errors: List[String]
  ):
    def +(t: PgType) = copy(pgTypes = t :: pgTypes)
    def ++(ts: List[PgType]) = ts.foldLeft(this)(_ + _)
    def +(t: GrackleType) = copy(grackleTypes = t :: grackleTypes)
    def +(s: String) = copy(errors = s :: errors)
    def ++(ss: List[String])(using DummyImplicit) = ss.foldLeft(this)(_ + _)

  object DiagState:
    val Initial = DiagState(Nil, Nil, Nil)

  def apply[F[_]: Concurrent: Logger](db: Session[F], schema: Schema, enums: Enums): StartupDiagnostics[F] =
    new StartupDiagnostics[F]:
      import enums.{ schema => _, * }

      val allDiagnostics: List[StateT[F, DiagState, Unit]] =
        List(

          // Postgres enums that have corresponding Scala enums, mediated through codecs
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

          // Postgres lookup tables that have corresponding Scala enums, mediated through codecs
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
          checkPostgresLookupTable(gmos_grating_order, "t_gmos_disperser_order"),
          checkPostgresLookupTable(gmos_north_detector, "t_gmos_north_detector"),
          checkPostgresLookupTable(gmos_north_filter, "t_gmos_north_filter"),
          checkPostgresLookupTable(gmos_north_fpu, "t_gmos_north_fpu"),
          checkPostgresLookupTable(gmos_north_grating, "t_gmos_north_disperser"),
          checkPostgresLookupTable(gmos_north_stage_mode, "t_gmos_north_stage_mode"),
          checkPostgresLookupTable(gmos_roi, "t_gmos_roi"),
          checkPostgresLookupTable(gmos_south_detector, "t_gmos_south_detector"),
          checkPostgresLookupTable(gmos_south_filter, "t_gmos_south_filter"),
          checkPostgresLookupTable(gmos_south_fpu, "t_gmos_south_fpu"),
          checkPostgresLookupTable(gmos_south_grating, "t_gmos_south_disperser"),
          checkPostgresLookupTable(gmos_south_stage_mode, "t_gmos_south_stage_mode"),
          checkPostgresLookupTable(image_quality_preset, "t_image_quality"),
          checkPostgresLookupTable(instrument, "t_instrument"),
          checkPostgresLookupTable(spectroscopy_capabilities, "t_spectroscopy_capabilities"),
          checkPostgresLookupTable(step_execution_state, "t_step_execution_state"),
          checkPostgresLookupTable(water_vapor, "t_water_vapor"),

          // Postgres enums that have no corresponding Scala enum
          assertUnmappedPostgresEnum(PgType("e_target_type")),
          assertUnmappedPostgresEnum(PgType("e_source_profile_type")),

          // GraphQL enums that have corresponding Scala enums          
          checkSchemaEnum[ArcType]("ArcType"),
          checkSchemaEnum[AtomExecutionState]("AtomExecutionState"),
          checkSchemaEnum[AtomStage]("AtomStage"),
          checkSchemaEnum[AttachmentType]("AttachmentType"),
          checkSchemaEnum[Band]("Band"),
          checkSchemaEnum[BlindOffsetType]("BlindOffsetType"),
          checkSchemaEnum[Breakpoint]("Breakpoint"),
          checkSchemaEnum[Units Of Brightness[Integrated]]("BrightnessIntegratedUnits"),
          checkSchemaEnum[Units Of Brightness[Surface]]("BrightnessSurfaceUnits"),
          checkSchemaEnum[CalculationState]("CalculationState"),
          checkSchemaEnum[CalibrationRole]("CalibrationRole"),
          checkSchemaEnum[CallForProposalsType]("CallForProposalsType"),
          checkSchemaEnum[CatalogName]("CatalogName"),
          checkSchemaEnum[ChargeClass]("ChargeClass"),
          checkSchemaEnum[CloudExtinction.Preset]("CloudExtinctionPreset"),
          checkSchemaEnum[ConfigurationRequestStatus]("ConfigurationRequestStatus"),
          checkSchemaEnum[CoolStarTemperature]("CoolStarTemperature"),
          checkSchemaEnum[DatabaseOperation]("DatabaseOperation"),
          checkSchemaEnum[DatasetQaState]("DatasetQaState"),
          checkSchemaEnum[DatasetStage]("DatasetStage"),
          checkSchemaEnum[EditType]("EditType"),
          checkSchemaEnum[EducationalStatus]("EducationalStatus"),
          checkSchemaEnum[EmailStatus]("EmailStatus"),
          checkSchemaEnum[EphemerisKeyType]("EphemerisKeyType"),
          checkSchemaEnum[ExecutionEventType]("ExecutionEventType"),
          checkSchemaEnum[ExecutionState]("ExecutionState"),
          checkSchemaEnum[Existence]("Existence"),
          checkSchemaEnum[FilterType]("FilterType", false),
          checkSchemaEnum[Flamingos2CustomSlitWidth]("Flamingos2CustomSlitWidth"),
          checkSchemaEnum[Flamingos2Decker]("Flamingos2Decker"),
          checkSchemaEnum[Flamingos2Disperser]("Flamingos2Disperser"),
          checkSchemaEnum[Flamingos2Filter]("Flamingos2Filter"),
          checkSchemaEnum[Flamingos2Fpu]("Flamingos2Fpu"),
          checkSchemaEnum[Flamingos2LyotWheel]("Flamingos2LyotWheel"),
          checkSchemaEnum[Flamingos2ReadMode]("Flamingos2ReadMode"),
          checkSchemaEnum[Flamingos2ReadoutMode]("Flamingos2ReadoutMode"),
          checkSchemaEnum[Flamingos2Reads]("Flamingos2Reads"),
          checkSchemaEnum[Units Of FluxDensityContinuum[Integrated]]("FluxDensityContinuumIntegratedUnits"),
          checkSchemaEnum[Units Of FluxDensityContinuum[Surface]]("FluxDensityContinuumSurfaceUnits"),
          checkSchemaEnum[FocalPlane]("FocalPlane"),
          checkSchemaEnum[GalaxySpectrum]("GalaxySpectrum"),
          checkSchemaEnum[GcalArc]("GcalArc"),
          checkSchemaEnum[GcalContinuum]("GcalContinuum"),
          checkSchemaEnum[GcalDiffuser]("GcalDiffuser"),
          checkSchemaEnum[GcalFilter]("GcalFilter"),
          checkSchemaEnum[GcalShutter]("GcalShutter"),
          checkSchemaEnum[Gender]("Gender"),
          checkSchemaEnum[GmosAmpCount]("GmosAmpCount"),
          checkSchemaEnum[GmosAmpGain]("GmosAmpGain"),
          checkSchemaEnum[GmosAmpReadMode]("GmosAmpReadMode"),
          checkSchemaEnum[GmosBinning]("GmosBinning"),
          checkSchemaEnum[GmosCustomSlitWidth]("GmosCustomSlitWidth"),
          checkSchemaEnum[GmosDtax]("GmosDtax"),
          checkSchemaEnum[GmosEOffsetting]("GmosEOffsetting"),
          checkSchemaEnum[GmosGratingOrder]("GmosGratingOrder"),
          checkSchemaEnum[GmosLongSlitAcquisitionRoi]("GmosLongSlitAcquisitionRoi"),
          checkSchemaEnum[GmosNorthFpu]("GmosNorthBuiltinFpu"),
          checkSchemaEnum[GmosNorthDetector]("GmosNorthDetector"),
          checkSchemaEnum[GmosNorthFilter]("GmosNorthFilter"),
          checkSchemaEnum[GmosNorthGrating]("GmosNorthGrating"),
          checkSchemaEnum[GmosNorthStageMode]("GmosNorthStageMode"),
          checkSchemaEnum[GmosRoi]("GmosRoi"),
          checkSchemaEnum[GmosSouthFpu]("GmosSouthBuiltinFpu"),
          checkSchemaEnum[GmosSouthDetector]("GmosSouthDetector"),
          checkSchemaEnum[GmosSouthFilter]("GmosSouthFilter"),
          checkSchemaEnum[GmosSouthGrating]("GmosSouthGrating"),
          checkSchemaEnum[GmosSouthStageMode]("GmosSouthStageMode"),
          checkSchemaEnum[GuideProbe]("GuideProbe"),
          checkSchemaEnum[StepGuideState]("GuideState"),
          checkSchemaEnum[ImageQuality.Preset]("ImageQualityPreset"),
          checkSchemaEnum[Instrument]("Instrument"),
          checkSchemaEnum[MosPreImaging]("MosPreImaging"),
          checkSchemaEnum[MultipleFiltersMode]("MultipleFiltersMode"),
          checkSchemaEnum[ObsActiveStatus]("ObsActiveStatus"),
          checkSchemaEnum[ObsStatus]("ObsStatus"),
          checkSchemaEnum[ObservationValidationCode]("ObservationValidationCode"),
          checkSchemaEnum[ObservationWorkflowState]("ObservationWorkflowState"),
          checkSchemaEnum[ObserveClass]("ObserveClass"),
          checkSchemaEnum[ObservingModeType]("ObservingModeType"),
          checkSchemaEnum[Partner]("Partner"),
          checkSchemaEnum[PartnerLinkType]("PartnerLinkType"),
          checkSchemaEnum[PlanetSpectrum]("PlanetSpectrum"),
          checkSchemaEnum[PlanetaryNebulaSpectrum]("PlanetaryNebulaSpectrum"),
          checkSchemaEnum[PosAngleConstraintMode]("PosAngleConstraintMode"),
          checkSchemaEnum[ProgramType]("ProgramType"),
          checkSchemaEnum[ProgramUserRole]("ProgramUserRole"),
          checkSchemaEnum[ProposalStatus]("ProposalStatus"),
          checkSchemaEnum[QuasarSpectrum]("QuasarSpectrum"),
          checkSchemaEnum[ScienceBand]("ScienceBand"),
          checkSchemaEnum[ScienceMode]("ScienceMode"),
          checkSchemaEnum[ScienceSubtype]("ScienceSubtype"),
          checkSchemaEnum[SequenceCommand]("SequenceCommand"),
          checkSchemaEnum[SequenceType]("SequenceType"),
          checkSchemaEnum[Site]("Site"),
          checkSchemaEnum[SkyBackground]("SkyBackground"),
          checkSchemaEnum[SlewStage]("SlewStage"),
          checkSchemaEnum[SmartGcalType]("SmartGcalType"),
          checkSchemaEnum[SpectroscopyCapabilities]("SpectroscopyCapabilities"),
          checkSchemaEnum[StellarLibrarySpectrum]("StellarLibrarySpectrum"),
          checkSchemaEnum[StepExecutionState]("StepExecutionState"),
          checkSchemaEnum[StepStage]("StepStage"),
          checkSchemaEnum[StepType]("StepType"),
          checkSchemaEnum[TacCategory]("TacCategory"),
          checkSchemaEnum[TargetDisposition]("TargetDisposition"),
          checkSchemaEnum[TimeAccountingCategory]("TimeAccountingCategory"),
          checkSchemaEnum[TimeChargeCorrection.Op]("TimeChargeCorrectionOp"),
          checkSchemaEnum[TimingWindowInclusion]("TimingWindowInclusion"),
          checkSchemaEnum[ToOActivation]("ToOActivation"),
          checkSchemaEnum[InvitationStatus]("UserInvitationStatus"),
          checkSchemaEnum[UserType]("UserType"),
          checkSchemaEnum[WaterVapor]("WaterVapor"),

          // GraphQL enums that have no corresponding Scala enum
          assertUnmappedSchemaEnum("SeeingTrend"),
          assertUnmappedSchemaEnum("Ignore"),
          assertUnmappedSchemaEnum("ConditionsExpectationType"),
          assertUnmappedSchemaEnum("ConditionsMeasurementSource"),
          assertUnmappedSchemaEnum("LineFluxIntegratedUnits"), // Probably should an enum for this
          assertUnmappedSchemaEnum("LineFluxSurfaceUnits"),    // Probably should an enum for this
          assertUnmappedSchemaEnum("TelluricTag"),
          assertUnmappedSchemaEnum("HiiRegionSpectrum"),       // Probably should an enum for this

          // These should come last, to see what we missed above.
          checkEnumCoverage, 
          checkSchemaCoverage,

        )

      def runAllDiagnostics(fatal: Boolean): F[Unit] =        
        Logger[F].info("Running startup diagnostics.") >>
        allDiagnostics.sequence.runS(DiagState.Initial).map(_.errors).flatMap: errors =>
          errors.traverse_(Logger[F].error(_)) >>
            Logger[F].info("Startup diagnostics passed.").whenA(errors.isEmpty) >>
            Logger[F].error("Startup diagnostics failed.").whenA(errors.nonEmpty) >>
            Concurrent[F].raiseError(new Error(s"Startup diagnostics failed. Exiting.")).whenA(fatal && errors.nonEmpty)

      def assertUnmappedPostgresEnum(tpe: PgType): StateT[F, DiagState, Unit] =
        StateT.modify(_ + tpe)

      def checkPostgresEnum[E: Enumerated](codec: Codec[E]): StateT[F, DiagState, Unit] =
        StateT.modifyF: ds =>
          db.stream(sql"SELECT unnest(enum_range(NULL::#${codec.types.head.name})::_varchar) as value".query(varchar))(skunk.Void, 104)
            .compile
            .toList
            .map(alignCodec(codec, "the database schema"))
            .map(ss => ds ++ codec.types ++ ss)

      def checkPostgresLookupTable[E: Enumerated: TypeName](codec: Codec[E], tableName: String): StateT[F, DiagState, Unit] =
        StateT.modifyF: ds =>
          db.stream(sql"SELECT c_tag FROM #$tableName".query(varchar))(skunk.Void, 104)
            .compile
            .toList
            .map(alignCodec(codec, s"lookup table $tableName"))
            .map(ss => ds ++ codec.types ++ ss)

      def checkEnumCoverage: StateT[F, DiagState, Unit] =
        StateT.modifyF: ds =>
          db.stream(sql"SELECT typname::varchar FROM pg_type WHERE typtype = 'e'".query(varchar))(skunk.Void, 104)
            .compile
            .toList
            .map: es =>
              val seen = ds.pgTypes.map(_.name).toSet
              val errors = es.filterNot(seen).map(s => s"Postgres enum $s has not been checked for consistency with a Scala enum.")
              ds ++ errors

      def checkSchemaEnum[A: Enumerated: TypeName](name: String, screaming: Boolean = true): StateT[F, DiagState, Unit] =
        StateT.modify: ds =>          
          schema.types.collectFirst { case t @ EnumType(`name`, _, _, _) => t } match
            case None => ds + s"GraphQL enum $name was not found in the Schema."
            case Some(et) => ds + et ++ alignSchema(s"GraphQL enum $name", screaming)(et.enumValues.map(_.name))

      def assertUnmappedSchemaEnum(name: String): StateT[F, DiagState, Unit] =
        StateT.modify: ds =>          
          schema.types.collectFirst { case t @ EnumType(`name`, _, _, _) => t } match
            case None => ds + s"GraphQL enum $name was not found in the Schema."
            case Some(et) => ds + et

      def checkSchemaCoverage: StateT[F, DiagState, Unit] =
        StateT.modify: ds =>          
          schema
            .types
            .filter(tpe => tpe.isEnum && !ds.grackleTypes.contains(tpe))
            .map(tpe => s"GraphQL enum ${tpe.name} has not been checked for consistency with a Scala enum.")
            .foldLeft(ds)(_ + _)

      /**
       * In the case of `Site` and maybe other places the Scala side tag isn't the same as the database side tag, and this
       * is mediated through the codec. So what we do is construct the element from its Scala tag and serialize it to get
       * its database tag.
       */
      def databaseTag[A](codec: Codec[A])(tag: String)(using e: Enumerated[A]): String =
        e.fromTag(tag).flatMap(codec.encode(_).headOption.flatten).getOrElse(tag)

      def align[A](where: String, transform: String => String)(found: List[String])(using e: Enumerated[A], tn: TypeName[A]): List[String] =
        val expected = e.all.map(e.tag).map(transform).toSet
        val actual   = found.toSet
        (expected ++ actual)
          .toList
          .sorted
          .flatMap: tag =>
            (expected(tag), actual(tag)) match
              case (true, false)  => List(s"${tn.value}: expected tag \"$tag\" was not found in $where.")
              case (false, true)  => List(s"${tn.value}: unexpected tag \"$tag\" was found in $where.")
              case _ => Nil

      def alignCodec[A](codec: Codec[A], where: String)(found: List[String])(using e: Enumerated[A], tn: TypeName[A]): List[String] =
        align(where, databaseTag(codec))(found)

      def alignSchema[A](where: String, screaming: Boolean)(found: List[String])(using e: Enumerated[A], tn: TypeName[A]): List[String] =
        align(where, s => if screaming then s.toScreamingSnakeCase else s)(found)
