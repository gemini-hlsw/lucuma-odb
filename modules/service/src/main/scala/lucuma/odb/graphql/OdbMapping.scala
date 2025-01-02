// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import _root_.skunk.AppliedFragment
import _root_.skunk.Session
import cats.Monoid
import cats.Parallel
import cats.effect.std.Supervisor
import cats.effect.{Unique as _, *}
import cats.syntax.all.*
import com.github.vertical_blank.sqlformatter.SqlFormatter
import fs2.concurrent.Topic
import grackle.*
import grackle.QueryCompiler.SelectElaborator
import grackle.skunk.SkunkMapping
import grackle.skunk.SkunkMonitor
import lucuma.core.model.User
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.graphql.mapping.*
import lucuma.odb.graphql.topic.ConfigurationRequestTopic
import lucuma.odb.graphql.topic.GroupTopic
import lucuma.odb.graphql.topic.ObservationTopic
import lucuma.odb.graphql.topic.ProgramTopic
import lucuma.odb.graphql.topic.TargetTopic
import lucuma.odb.graphql.util.*
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services
import lucuma.odb.util.Codecs.DomainCodec
import natchez.Trace
import org.http4s.client.Client
import org.tpolecat.sourcepos.SourcePos
import org.typelevel.log4cats.Logger

import scala.io.AnsiColor
import scala.io.Source

object OdbMapping {

  case class Topics[F[_]](
    program:     Topic[F, ProgramTopic.Element],
    observation: Topic[F, ObservationTopic.Element],
    target:      Topic[F, TargetTopic.Element],
    group:       Topic[F, GroupTopic.Element],
    configurationRequest: Topic[F, ConfigurationRequestTopic.Element],
  )

  object Topics {
    def apply[F[_]: Concurrent: Logger](pool: Resource[F, Session[F]]): Resource[F, Topics[F]] =
      for {
        sup <- Supervisor[F]
        ses <- pool
        pro <- Resource.eval(ProgramTopic(ses, 1024, sup))
        obs <- Resource.eval(ObservationTopic(ses, 1024, sup))
        tar <- Resource.eval(TargetTopic(ses, 1024, sup))
        grp <- Resource.eval(GroupTopic(ses, 1024, sup))
        cr  <- Resource.eval(ConfigurationRequestTopic(ses, 1024, sup))
      } yield Topics(pro, obs, tar, grp, cr)
  }

  // Loads a GraphQL file from the classpath, relative to this Class.
  def unsafeLoadSchema(fileName: String): Schema = {
    val stream = getClass.getResourceAsStream(fileName)
    val src  = Source.fromInputStream(stream, "UTF-8")
    try Schema(src.getLines().mkString("\n")).toEither.fold(x => sys.error(s"Invalid schema: $fileName: ${x.toList.mkString(", ")}"), identity)
    finally src.close()
  }

  private implicit def monoidPartialFunction[A, B]: Monoid[PartialFunction[A, B]] =
    Monoid.instance(PartialFunction.empty, _ orElse _)

  def apply[F[_]: Async: Parallel: Trace: Logger](
    database:      Resource[F, Session[F]],
    monitor0:      SkunkMonitor[F],
    user0:         User,
    topics0:       Topics[F],
    itcClient0:    ItcClient[F],
    commitHash0:   CommitHash,
    enums:         Enums,
    tec:           TimeEstimateCalculatorImplementation.ForInstrumentMode,
    httpClient0:   Client[F],
    emailConfig0:  Config.Email,
    allowSub:      Boolean = true,       // Are submappings (recursive calls) allowed?
    schema0:       Option[Schema] = None // If we happen to have a schema we can pass it and avoid more parsing
  ): Mapping[F] =
        new SkunkMapping[F](database, monitor0)
          with BaseMapping[F]
          with AddAtomEventResultMapping[F]
          with AddConditionsEntryResultMapping[F]
          with AddDatasetEventResultMapping[F]
          with AddSequenceEventResultMapping[F]
          with AddSlewEventResultMapping[F]
          with AddStepEventResultMapping[F]
          with AddTimeChargeCorrectionResultMapping[F]
          with AirMassRangeMapping[F]
          with AllocationMapping[F]
          with AngleMapping[F]
          with AsterismGroupMapping[F]
          with AsterismGroupSelectResultMapping[F]
          with AtomRecordMapping[F]
          with AtomRecordSelectResultMapping[F]
          with CallForProposalsMapping[F]
          with CallsForProposalsSelectResultMapping[F]
          with CatalogInfoMapping[F]
          with CategorizedTimeMapping[F]
          with CloneGroupResultMapping[F]
          with CloneObservationResultMapping[F]
          with CloneTargetResultMapping[F]
          with ConditionsEntryMapping[F]
          with ConditionsExpectationMapping[F]
          with ConditionsIntuitionMapping[F]
          with ConditionsMeasurementMapping[F]
          with ConfigurationConditionsMapping[F]
          with ConfigurationGmosLongSlitMappings[F]
          with ConfigurationMapping[F]
          with ConfigurationRequestMapping[F]
          with ConfigurationRequestEditMapping[F]
          with ConfigurationRequestSelectResultMapping[F]
          with ConfigurationObservingModeMappings[F]
          with ConstraintSetGroupMapping[F]
          with ConstraintSetGroupSelectResultMapping[F]
          with ConstraintSetMapping[F]
          with CoordinateLimitsMapping[F]
          with CoordinatesMapping[F]
          with CreateCallForProposalsResultMapping[F]
          with CreateGroupResultMapping[F]
          with CreateObservationResultMapping[F]
          with AddProgramUserResultMapping[F]
          with CreateProgramResultMapping[F]
          with CreateProposalResultMapping[F]
          with CreateTargetResultMapping[F]
          with CreateUserInvitationResultMapping[F]
          with DatasetMapping[F]
          with DatasetReferenceMapping[F]
          with DatasetSelectResultMapping[F]
          with DateIntervalMapping[F]
          with DeclinationMapping[F]
          with ElevationRangeMapping[F]
          with EmailMapping[F]
          with ExecutionMapping[F]
          with ExecutionEventMapping[F]
          with ExecutionEventSelectResultMapping[F]
          with FilterTypeMetaMapping[F]
          with GmosCcdModeMapping[F]
          with GmosCustomMaskMapping[F]
          with GmosDynamicMapping[F]
          with GmosFpuMapping[F]
          with GmosGratingConfigMapping[F]
          with GmosLongSlitMapping[F]
          with GmosNorthStaticMapping[F]
          with GmosSouthStaticMapping[F]
          with GoaPropertiesMapping[F]
          with GroupMapping[F]
          with GroupEditMapping[F]
          with GroupElementMapping[F]
          with HourAngleRangeMapping[F]
          with LeafMappings[F]
          with LinkUserResultMapping[F]
          with MutationMapping[F]
          with NonsiderealMapping[F]
          with AttachmentMapping[F]
          with ObservationEditMapping[F]
          with ObservationMapping[F]
          with ObservationReferenceMapping[F]
          with ObservationSelectResultMapping[F]
          with ObservingModeGroupMapping[F]
          with ObservingModeGroupSelectResultMapping[F]
          with ObservingModeMapping[F]
          with OffsetMapping[F]
          with ParallaxMapping[F]
          with PartnerSplitMapping[F]
          with PosAngleConstraintMapping[F]
          with ProgramEditMapping[F]
          with ProgramMapping[F]
          with ProgramReferenceMapping[F]
          with ProgramSelectResultMapping[F]
          with ProgramUserMapping[F]
          with ProgramUserSelectResultMapping[F]
          with ProperMotionDeclinationMapping[F]
          with ProperMotionMapping[F]
          with ProperMotionRaMapping[F]
          with ProposalMapping[F]
          with ProposalReferenceMapping[F]
          with ProposalStatusMetaMapping[F]
          with ProposalTypeMapping[F]
          with QueryMapping[F]
          with RadialVelocityMapping[F]
          with RecordDatasetResultMapping[F]
          with RecordAtomResultMapping[F]
          with RecordGmosNorthVisitResultMapping[F]
          with RecordGmosSouthVisitResultMapping[F]
          with RecordGmosStepResultMapping[F]
          with RedeemUserInvitationResultMapping[F]
          with RevokeUserInvitationResultMapping[F]
          with RightAscensionMapping[F]
          with ScienceRequirementsMapping[F]
          with SetAllocationsResultMapping[F]
          with SetGuideTargetNameResultMapping[F]
          with SetProgramReferenceResultMapping[F]
          with SetProposalStatusResultMapping[F]
          with SiderealMapping[F]
          with SiteCoordinateLimitsMapping[F]
          with SpectroscopyConfigOptionMapping[F]
          with SpectroscopyScienceRequirementsMapping[F]
          with StepConfigMapping[F]
          with StepRecordMapping[F]
          with StepRecordSelectResultMapping[F]
          with SubscriptionMapping[F]
          with TargetEditMapping[F]
          with TargetEnvironmentMapping[F]
          with TargetMapping[F]
          with TargetGroupMapping[F]
          with TargetGroupSelectResultMapping[F]
          with TargetSelectResultMapping[F]
          with TelescopeConfigMapping[F]
          with TimeChargeCorrectionMapping[F]
          with TimeChargeDiscountMapping[F]
          with TimeChargeInvoiceMapping[F]
          with TimeSpanMapping[F]
          with TimingWindowMappings[F]
          with UpdateAsterismsResultMapping[F]
          with UpdateCallsForProposalsResultMapping[F]
          with UpdateConfigurationRequestsResultMapping[F]
          with UpdateDatasetsResultMapping[F]
          with UpdateGroupsResultMapping[F]
          with UpdateAttachmentsResultMapping[F]
          with UpdateObservationsResultMapping[F]
          with UpdateProgramsResultMapping[F]
          with UpdateProgramUsersResultMapping[F]
          with UpdateProposalResultMapping[F]
          with UpdateTargetsResultMapping[F]
          with UserInvitationMapping[F]
          with UserMapping[F]
          with UserProfileMapping[F]
          with VisitMapping[F]
          with VisitSelectResultMapping[F]
          with WavelengthMapping[F]
        {

          // By default there is a tight (5 level) limit on the nesting of
          // objects in inputs.  We need to increase it to handle our schema.
          override def parserConfig: GraphQLParser.Config =
            GraphQLParser
              .defaultConfig
              .copy(maxInputValueDepth = 16)

          // Our schema
          val schema: Schema =
            schema0.getOrElse(unsafeLoadSchema("OdbSchema.graphql") |+| enums.schema)

          // Our services and resources needed by various mappings.
          override val commitHash = commitHash0
          override val itcClient = itcClient0
          override val user: User = user0
          override val topics: Topics[F] = topics0

          override val services: Resource[F, Services[F]] = 
            pool.map: session =>
              Services.forUser(
                user, 
                enums,
                Option.when(allowSub)(apply(
                  Resource.pure(session), // Always use this session
                  monitor0,               // Same args as the outer mapping
                  user0,
                  topics0,
                  itcClient0,
                  commitHash0,
                  enums,
                  tec,
                  httpClient0,
                  emailConfig0,            
                  false,                  // don't allow further sub-mappings; only one level of recursion is allowed
                  Some(schema),           // don't re-parse the schema
                ))
              )(session)

          override val timeEstimateCalculator: TimeEstimateCalculatorImplementation.ForInstrumentMode = tec
          override val httpClient: Client[F] = httpClient0
          override val emailConfig: Config.Email = emailConfig0

          // Our combined type mappings
          override val typeMappings: TypeMappings =
            TypeMappings(
              List[TypeMapping](
                AddAtomEventResultMapping,
                AddConditionsEntryResultMapping,
                AddDatasetEventResultMapping,
                AddSequenceEventResultMapping,
                AddSlewEventResultMapping,
                AddStepEventResultMapping,
                AddTimeChargeCorrectionResultMapping,
                AirMassRangeMapping,
                AllocationMapping,
                AsterismGroupMapping,
                AsterismGroupSelectResultMapping,
                AtomEventMapping,
                AtomRecordMapping,
                AttachmentMapping,
                CalibrationProgramReferenceMapping,
                CallForProposalsMapping,
                CallForProposalsPartnerMapping,
                CallsForProposalsSelectResultMapping,
                CatalogInfoMapping,
                ClassicalMapping,
                CloneGroupResultMapping,
                CloneObservationResultMapping,
                CloneTargetResultMapping,
                CommissioningProgramReferenceMapping,
                ConditionsEntryMapping,
                ConditionsExpectationMapping,
                ConditionsIntuitionMapping,
                ConditionsMeasurementMapping,
                ConfigurationRequestMapping,
                ConfigurationRequestEditMapping,
                ConfigurationRequestSelectResultMapping,
                ConstraintSetGroupMapping,
                ConstraintSetGroupSelectResultMapping,
                ConstraintSetMapping,
                CreateCallForProposalsResultMapping,
                CreateGroupResultMapping,
                CreateObservationResultMapping,
                AddProgramUserResultMapping,
                CreateProgramResultMapping,
                CreateProposalResultMapping,
                CreateTargetResultMapping,
                CreateUserInvitationResultMapping,
                DatasetEventMapping,
                DatasetMapping,
                DatasetReferenceMapping,
                DemoScienceMapping,
                DirectorsTimeMapping,
                EmailMapping,
                EngineeringProgramReferenceMapping,
                ElevationRangeMapping,
                ExampleProgramReferenceMapping,
                ExecutionMapping,
                ExecutionEventMapping,
                FastTurnaroundMapping,
                FilterTypeMetaMapping,
                GmosNorthLongSlitMapping,
                GmosNorthStaticMapping,
                GmosSouthLongSlitMapping,
                GmosSouthStaticMapping,
                GoaPropertiesMapping,
                GroupMapping,
                GroupEditMapping,
                GroupElementMapping,
                HourAngleRangeMapping,
                LargeProgramMapping,
                LibraryProgramReferenceMapping,
                LinkUserResultMapping,
                MonitoringProgramReferenceMapping,
                MutationMapping,
                NonsiderealMapping,
                ObservationEditMapping,
                ObservationMapping,
                ObservationReferenceMapping,
                ObservingModeGroupMapping,
                ObservingModeGroupSelectResultMapping,
                ObservingModeMapping,
                ParallaxMapping,
                PartnerSplitMapping,
                PoorWeatherMapping,
                PosAngleConstraintMapping,
                ProgramMapping,
                ProgramEditMapping,
                ProgramReferenceMapping,
                ProgramSelectResultMapping,
                ProgramUserMapping,
                ProgramUserSelectResultMapping,
                ProperMotionDeclinationMapping,
                ProperMotionMapping,
                ProperMotionRaMapping,
                ProposalMapping,
                ProposalReferenceMapping,
                ProposalStatusMetaMapping,
                ProposalTypeMapping,
                QueryMapping,
                QueueMapping,
                RadialVelocityMapping,
                RecordAtomResultMapping,
                RecordDatasetResultMapping,
                RecordGmosNorthStepResultMapping,
                RecordGmosNorthVisitResultMapping,
                RecordGmosSouthStepResultMapping,
                RecordGmosSouthVisitResultMapping,
                RedeemUserInvitationResultMapping,
                RevokeUserInvitationResultMapping,
                ScienceProgramReferenceMapping,
                ScienceRequirementsMapping,
                SequenceEventMapping,
                SpectroscopyConfigOptionMapping,
                SpectroscopyConfigOptionGmosNorthMapping,
                SpectroscopyConfigOptionGmosSouthMapping,
                SpectroscopyScienceRequirementsMapping,
                SetAllocationsResultMapping,
                SetGuideTargetNameResultMapping,
                SetProgramReferenceResultMapping,
                SetProposalStatusResultMapping,
                SiderealMapping,
                SlewEventMapping,
                StepConfigMapping,
                StepConfigBiasMapping,
                StepConfigDarkMapping,
                StepConfigGcalMapping,
                StepConfigScienceMapping,
                StepConfigSmartGcalMapping,
                StepEventMapping,
                StepRecordMapping,
                StepRecordSelectResultMapping,
                SubscriptionMapping,
                SystemProgramReferenceMapping,
                SystemVerificationMapping,
                TargetEditMapping,
                TargetEnvironmentMapping,
                TargetGroupMapping,
                TargetGroupSelectResultMapping,
                TargetMapping,
                TargetSelectResultMapping,
                TelescopeConfigMapping,
                TimeChargeCorrectionMapping,
                TimeChargeDaylightDiscountMapping,
                TimeChargeNoDataDiscountMapping,
                TimeChargeQaDiscountMapping,
                TimeChargeDiscountMapping,
                TimeChargeInvoiceMapping,
                TimingWindowEndAfterMapping,
                TimingWindowEndAtMapping,
                TimingWindowEndMapping,
                TimingWindowMapping,
                TimingWindowRepeatMapping,
                UpdateAsterismsResultMapping,
                UpdateAttachmentsResultMapping,
                UpdateCallsForProposalsResultMapping,
                UpdateConfigurationRequestsResultMapping,
                UpdateDatasetsResultMapping,
                UpdateGroupsResultMapping,
                UpdateObservationsResultMapping,
                UpdateProgramsResultMapping,
                UpdateProgramUsersResultMapping,
                UpdateProposalResultMapping,
                UpdateTargetsResultMapping,
                UserInvitationMapping,
                UserMapping,
                VisitMapping,
                VisitSelectResultMapping,
              ) ++ List(
                AngleMappings,
                AtomRecordSelectResultMappings,
                CategorizedTimeMappings,
                ConfigurationMappings,
                ConfigurationConditionsMappings,
                ConfigurationGmosNorthLongSlitMappings,
                ConfigurationGmosSouthLongSlitMappings,
                ConfigurationObservingModeMappings,
                CoordinatesMappings,
                CoordinateLimitsMappings,
                DatasetSelectResultMappings,
                DateIntervalMappings,
                DeclinationMapping,
                ExecutionEventSelectResultMapping,
                GmosCcdModeMappings,
                GmosCustomMaskMapping,
                GmosNorthDynamicMappings,
                GmosNorthFpuMappings,
                GmosNorthGratingConfigMappings,
                GmosSouthDynamicMappings,
                GmosSouthFpuMappings,
                GmosSouthGratingConfigMappings,
                LeafMappings,
                ObservationSelectResultMappings,
                OffsetMappings,
                OffsetPMappings,
                OffsetQMappings,
                RightAscensionMappings,
                SiteCoordinateLimitsMappings,
                TimeSpanMappings,
                UserProfileMappings,
                WavelengthMappings
              ).flatten

            )

          // Our combined select elaborator
          override val selectElaborator: SelectElaborator =
            SelectElaborator(
              List(
                AsterismGroupElaborator,
                AtomRecordElaborator,
                CallForProposalsElaborator,
                ProposalTypeElaborator,
                ConstraintSetGroupElaborator,
                DatasetElaborator,
                ExecutionElaborator,
                GroupElaborator,
                MutationElaborator,
                ObservationElaborator,
                ObservingModeGroupElaborator,
                ProgramElaborator,
                StepRecordElaborator,
                SubscriptionElaborator,
                TargetEnvironmentElaborator,
                TargetGroupElaborator,
                TimeChargeInvoiceElaborator,
                QueryElaborator,
                VisitElaborator
              ).combineAll
            )

          // Override `defaultRootCursor` to log the GraphQL query. This is optional.
          override def defaultRootCursor(query: Query, tpe: Type, parentCursor: Option[Cursor]): F[Result[(Query, Cursor)]] =
            Logger[F].debug("\n\n" + PrettyPrinter.query(query).render(100) + "\n") >>
            super.defaultRootCursor(query, tpe, parentCursor)

          // Override `fetch` to log the SQL query. This is optional.
          override def fetch(fragment: AppliedFragment, codecs: List[(Boolean, Codec)]): F[Vector[Array[Any]]] = {
            Logger[F].debug {
              val formatted = SqlFormatter.format(fragment.fragment.sql)
              val cleanedUp = formatted.replaceAll("\\$ (\\d+)", "\\$$1") // turn $ 42 into $42
              val colored   = cleanedUp.linesIterator.map(s => s"${AnsiColor.GREEN}$s${AnsiColor.RESET}").mkString("\n")
              s"\n\n$colored\n\n"
            } *>
            super.fetch(fragment, codecs)
          }

          // HACK: If the codec is a DomainCodec then use the domain name when generating `null::<type>` in Grackle queries
          override implicit def Fragments: SqlFragment[AppliedFragment] =
            val delegate = super.Fragments
            new SqlFragment[AppliedFragment]:
              export delegate.{ sqlTypeName => _, * }
              def sqlTypeName(codec: Codec): Option[String] =
                codec._1 match
                  case DomainCodec(name, _) => Some(name)
                  case _ => delegate.sqlTypeName(codec)

        }

  /**
   * A minimal read-only mapping that only knows how to return enum metadata. Other queries will
   * fail with errors.
   */
  def forMetadata[F[_]: Async: Trace: Logger](
    database: Resource[F, Session[F]],
    monitor:  SkunkMonitor[F],
    enums:    Enums,
  ): Mapping[F] =
    new SkunkMapping[F](database, monitor)
      with BaseMapping[F]
      with FilterTypeMetaMapping[F]
      with LeafMappings[F]
      with ProposalStatusMetaMapping[F]
      with QueryMapping[F]
    {

      // These are unused for enum metadata queries.
      def user = sys.error("OdbMapping.forMetadata: no user available")
      def services = sys.error("OdbMapping.forMetadata: no services available")

      // Our schema
      val schema: Schema =
        unsafeLoadSchema("OdbSchema.graphql") |+| enums.schema

      // Our combined type mappings
      override val typeMappings: TypeMappings =
        TypeMappings.unchecked(
          List(
          FilterTypeMetaMapping,
          ProposalStatusMetaMapping,
            QueryMapping,
          ) ++ LeafMappings
        )

      override val selectElaborator: SelectElaborator =
        SelectElaborator(QueryElaborator)

    }

}
