// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import _root_.skunk.AppliedFragment
import _root_.skunk.Session
import cats.Monoid
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
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.graphql.mapping.*
import lucuma.odb.graphql.topic.GroupTopic
import lucuma.odb.graphql.topic.ObservationTopic
import lucuma.odb.graphql.topic.ProgramTopic
import lucuma.odb.graphql.topic.TargetTopic
import lucuma.odb.graphql.util.*
import lucuma.odb.logic.TimeEstimateCalculator
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
      } yield Topics(pro, obs, tar, grp)
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

  def apply[F[_]: Async: Trace: Logger](
    database:    Resource[F, Session[F]],
    monitor:     SkunkMonitor[F],
    user0:       User,
    topics0:     Topics[F],
    itcClient0:  ItcClient[F],
    commitHash0: CommitHash,
    enums:       Enums,
    tec:         TimeEstimateCalculator.ForInstrumentMode,
    httpClient0:  Client[F]
  ):  Mapping[F] =
        new SkunkMapping[F](database, monitor)
          with BaseMapping[F]
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
          with CatalogInfoMapping[F]
          with CategorizedTimeMapping[F]
          with CloneObservationResultMapping[F]
          with CloneTargetResultMapping[F]
          with ConditionsEntryMapping[F]
          with ConditionsExpectationMapping[F]
          with ConditionsIntuitionMapping[F]
          with ConditionsMeasurementMapping[F]
          with ConstraintSetGroupMapping[F]
          with ConstraintSetGroupSelectResultMapping[F]
          with ConstraintSetMapping[F]
          with CoordinatesMapping[F]
          with CreateGroupResultMapping[F]
          with CreateObservationResultMapping[F]
          with CreateProgramResultMapping[F]
          with CreateTargetResultMapping[F]
          with CreateUserInvitationResultMapping[F]
          with DatasetMapping[F]
          with DatasetReferenceMapping[F]
          with DatasetSelectResultMapping[F]
          with DeclinationMapping[F]
          with ElevationRangeMapping[F]
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
          with GroupMapping[F]
          with GroupEditMapping[F]
          with GroupElementMapping[F]
          with HourAngleRangeMapping[F]
          with LeafMappings[F]
          with LinkUserResultMapping[F]
          with MutationMapping[F]
          with NonsiderealMapping[F]
          with ObsAttachmentFileExtMapping[F]
          with ObsAttachmentMapping[F]
          with ObsAttachmentTypeMetaMapping[F]
          with ObservationEditMapping[F]
          with ObservationMapping[F]
          with ObservationReferenceMapping[F]
          with ObservingModeMapping[F]
          with ObservationSelectResultMapping[F]
          with OffsetMapping[F]
          with ParallaxMapping[F]
          with PartnerMetaMapping[F]
          with PartnerSplitMapping[F]
          with PosAngleConstraintMapping[F]
          with ProgramEditMapping[F]
          with ProgramMapping[F]
          with ProgramReferenceMapping[F]
          with ProgramSelectResultMapping[F]
          with ProgramUserMapping[F]
          with ProperMotionDeclinationMapping[F]
          with ProperMotionMapping[F]
          with ProperMotionRaMapping[F]
          with ProposalMapping[F]
          with ProposalAttachmentMapping[F]
          with ProposalAttachmentTypeMetaMapping[F]
          with ProposalClassMapping[F]
          with ProposalReferenceMapping[F]
          with ProposalStatusMetaMapping[F]
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
          with SetAllocationResultMapping[F]
          with SetProgramReferenceResultMapping[F]
          with SiderealMapping[F]
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
          with TimeChargeCorrectionMapping[F]
          with TimeChargeDiscountMapping[F]
          with TimeChargeInvoiceMapping[F]
          with TimeSpanMapping[F]
          with TimingWindowMappings[F]
          with UpdateAsterismsResultMapping[F]
          with UpdateDatasetsResultMapping[F]
          with UpdateGroupsResultMapping[F]
          with UpdateObsAttachmentsResultMapping[F]
          with UpdateObservationsResultMapping[F]
          with UpdateProgramsResultMapping[F]
          with UpdateTargetsResultMapping[F]
          with UserMapping[F]
          with UserInvitationMapping[F]
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
            unsafeLoadSchema("OdbSchema.graphql") |+| enums.schema

          // Our services and resources needed by various mappings.
          override val commitHash = commitHash0
          override val itcClient = itcClient0
          override val user: User = user0
          override val topics: Topics[F] = topics0
          override val services: Resource[F, Services[F]] = pool.map(Services.forUser(user, enums))
          override val timeEstimateCalculator: TimeEstimateCalculator.ForInstrumentMode = tec
          override val httpClient: Client[F] = httpClient0

          // Our combined type mappings
          override val typeMappings: List[TypeMapping] =
            List(
              AddConditionsEntryResultMapping,
              AddDatasetEventResultMapping,
              AddSequenceEventResultMapping,
              AddSlewEventResultMapping,
              AddStepEventResultMapping,
              AddTimeChargeCorrectionResultMapping,
              AirMassRangeMapping,
              AllocationMapping,
              AngleMapping,
              AsterismGroupMapping,
              AsterismGroupSelectResultMapping,
              AtomRecordMapping,
              AtomRecordSelectResultMapping,
              CalibrationProgramReferenceMapping,
              CatalogInfoMapping,
              CategorizedTimeMapping,
              CloneObservationResultMapping,
              CloneTargetResultMapping,
              CommissioningProgramReferenceMapping,
              ConditionsEntryMapping,
              ConditionsExpectationMapping,
              ConditionsIntuitionMapping,
              ConditionsMeasurementMapping,
              ConstraintSetGroupMapping,
              ConstraintSetGroupSelectResultMapping,
              ConstraintSetMapping,
              CoordinatesMapping,
              CreateGroupResultMapping,
              CreateObservationResultMapping,
              CreateProgramResultMapping,
              CreateTargetResultMapping,
              CreateUserInvitationResultMapping,
              DatasetEventMapping,
              DatasetMapping,
              DatasetReferenceMapping,
              DatasetSelectResultMapping,
              DeclinationMapping,
              EngineeringProgramReferenceMapping,
              ElevationRangeMapping,
              ExampleProgramReferenceMapping,
              ExecutionMapping,
              ExecutionEventMapping,
              ExecutionEventSelectResultMapping,
              FilterTypeMetaMapping,
              GmosCcdModeMapping,
              GmosCustomMaskMapping,
              GmosNorthDynamicMapping,
              GmosNorthFpuMapping,
              GmosNorthGratingConfigMapping,
              GmosNorthLongSlitMapping,
              GmosNorthStaticMapping,
              GmosSouthDynamicMapping,
              GmosSouthFpuMapping,
              GmosSouthGratingConfigMapping,
              GmosSouthLongSlitMapping,
              GmosSouthStaticMapping,
              GroupMapping,
              GroupEditMapping,
              GroupElementMapping,
              HourAngleRangeMapping,
              LibraryProgramReferenceMapping,
              LinkUserResultMapping,
              MonitoringProgramReferenceMapping,
              MutationMapping,
              NonsiderealMapping,
              ObsAttachmentMapping,
              ObsAttachmentFileExtMapping,
              ObsAttachmentTypeMetaMapping,
              ObservationEditMapping,
              ObservationMapping,
              ObservationReferenceMapping,
              ObservationSelectResultMapping,
              ObservingModeMapping,
              OffsetMapping,
              OffsetPMapping,
              OffsetQMapping,
              ParallaxMapping,
              PartnerMetaMapping,
              PartnerSplitMapping,
              PosAngleConstraintMapping,
              ProgramMapping,
              ProgramEditMapping,
              ProgramReferenceMapping,
              ProgramSelectResultMapping,
              ProgramUserMapping,
              ProperMotionDeclinationMapping,
              ProperMotionMapping,
              ProperMotionRaMapping,
              ProposalAttachmentMapping,
              ProposalAttachmentTypeMetaMapping,
              ProposalMapping,
              ProposalReferenceMapping,
              ProposalStatusMetaMapping,
              QueryMapping,
              RadialVelocityMapping,
              RecordAtomResultMapping,
              RecordDatasetResultMapping,
              RecordGmosNorthStepResultMapping,
              RecordGmosNorthVisitResultMapping,
              RecordGmosSouthStepResultMapping,
              RecordGmosSouthVisitResultMapping,
              RedeemUserInvitationResultMapping,
              RevokeUserInvitationResultMapping,
              RightAscensionMapping,
              ScienceProgramReferenceMapping,
              ScienceRequirementsMapping,
              SequenceEventMapping,
              SpectroscopyConfigOptionMapping,
              SpectroscopyConfigOptionGmosNorthMapping,
              SpectroscopyConfigOptionGmosSouthMapping,
              SpectroscopyScienceRequirementsMapping,
              SetAllocationResultMapping,
              SetProgramReferenceResultMapping,
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
              TargetEditMapping,
              TargetEnvironmentMapping,
              TargetGroupMapping,
              TargetGroupSelectResultMapping,
              TargetMapping,
              TargetSelectResultMapping,
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
              TimeSpanMapping,
              UpdateAsterismsResultMapping,
              UpdateDatasetsResultMapping,
              UpdateGroupsResultMapping,
              UpdateObsAttachmentsResultMapping,
              UpdateObservationsResultMapping,
              UpdateProgramsResultMapping,
              UpdateTargetsResultMapping,
              UserMapping,
              UserInvitationMapping,
              VisitMapping,
              VisitSelectResultMapping,
              WavelengthMapping
            ) ++ LeafMappings ++ ProposalClassMappings

          // Our combined select elaborator
          override val selectElaborator: SelectElaborator =
            SelectElaborator(
              List(
                AsterismGroupElaborator,
                AtomRecordElaborator,
                ConstraintSetGroupElaborator,
                DatasetElaborator,
                ExecutionElaborator,
                GroupElaborator,
                MutationElaborator,
                ObservationElaborator,
                ProgramElaborator,
                ProposalElaborator,
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
      with ObsAttachmentTypeMetaMapping[F]
      with ObsAttachmentFileExtMapping[F]
      with PartnerMetaMapping[F]
      with ProposalAttachmentTypeMetaMapping[F]
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
      override val typeMappings: List[TypeMapping] =
        List(
          FilterTypeMetaMapping,
          ObsAttachmentTypeMetaMapping,
          ObsAttachmentFileExtMapping,
          PartnerMetaMapping,
          ProposalAttachmentTypeMetaMapping,
          ProposalStatusMetaMapping,
          QueryMapping,
        ) ++ LeafMappings

      override val selectElaborator: SelectElaborator =
        SelectElaborator(QueryElaborator)
    
    }
        
}
