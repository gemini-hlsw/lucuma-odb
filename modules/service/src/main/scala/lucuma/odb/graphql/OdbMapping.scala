// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import _root_.skunk.AppliedFragment
import _root_.skunk.Session
import cats.Monoid
import cats.effect.std.Supervisor
import cats.effect.{Unique => _, _}
import cats.syntax.all._
import com.github.vertical_blank.sqlformatter.SqlFormatter
import edu.gemini.grackle.QueryCompiler.SelectElaborator
import edu.gemini.grackle._
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.skunk.SkunkMonitor
import fs2.concurrent.Topic
import lucuma.core.model.User
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.graphql.mapping.CreateGroupResultMapping
import lucuma.odb.graphql.mapping.UpdateObservationsResultMapping
import lucuma.odb.graphql.mapping._
import lucuma.odb.graphql.topic.GroupTopic
import lucuma.odb.graphql.topic.ObservationTopic
import lucuma.odb.graphql.topic.ProgramTopic
import lucuma.odb.graphql.topic.TargetTopic
import lucuma.odb.graphql.util._
import lucuma.odb.logic.PlannedTimeCalculator
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
    ptc:         PlannedTimeCalculator.ForInstrumentMode,
    httpClient0:  Client[F]
  ):  Mapping[F] =
        new SkunkMapping[F](database, monitor)
          with BaseMapping[F]
          with AddConditionsEntryResultMapping[F]
          with AddDatasetEventResultMapping[F]
          with AddSequenceEventResultMapping[F]
          with AddStepEventResultMapping[F]
          with AirMassRangeMapping[F]
          with AllocationMapping[F]
          with AngleMapping[F]
          with AsterismGroupMapping[F]
          with AsterismGroupSelectResultMapping[F]
          with CatalogInfoMapping[F]
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
          with DeclinationMapping[F]
          with DatasetMapping[F]
          with DatasetEventMapping[F]
          with DatasetSelectResultMapping[F]
          with ElevationRangeMapping[F]
          with ExecutionMapping[F]
          with FilterTypeMetaMapping[F]
          with GmosAtomRecordMapping[F]
          with GmosCcdModeMapping[F]
          with GmosCustomMaskMapping[F]
          with GmosDynamicMapping[F]
          with GmosFpuMapping[F]
          with GmosGratingConfigMapping[F]
          with GmosLongSlitMapping[F]
          with GmosNorthStaticMapping[F]
          with GmosNorthVisitMapping[F]
          with GmosSouthStaticMapping[F]
          with GmosSouthVisitMapping[F]
          with GmosStepRecordMapping[F]
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
          with ObservingModeMapping[F]
          with ObservationSelectResultMapping[F]
          with OffsetMapping[F]
          with ParallaxMapping[F]
          with PartnerMetaMapping[F]
          with PartnerSplitMapping[F]
          with PlannedTimeSummaryMapping[F]
          with PosAngleConstraintMapping[F]
          with ProgramEditMapping[F]
          with ProgramMapping[F]
          with ProgramSelectResultMapping[F]
          with ProgramUserMapping[F]
          with ProperMotionDeclinationMapping[F]
          with ProperMotionMapping[F]
          with ProperMotionRaMapping[F]
          with ProposalMapping[F]
          with ProposalClassMapping[F]
          with ProposalAttachmentMapping[F]
          with ProposalAttachmentTypeMetaMapping[F]
          with QueryMapping[F]
          with RadialVelocityMapping[F]
          with RecordDatasetResultMapping[F]
          with RecordGmosAtomResultMapping[F]
          with RecordGmosNorthVisitResultMapping[F]
          with RecordGmosSouthVisitResultMapping[F]
          with RecordGmosStepResultMapping[F]
          with RightAscensionMapping[F]
          with ScienceRequirementsMapping[F]
          with SequenceEventMapping[F]
          with SetAllocationResultMapping[F]
          with SiderealMapping[F]
          with SpectroscopyScienceRequirementsMapping[F]
          with StepConfigMapping[F]
          with StepEventMapping[F]
          with SubscriptionMapping[F]
          with TargetEditMapping[F]
          with TargetEnvironmentMapping[F]
          with TargetMapping[F]
          with TargetGroupMapping[F]
          with TargetGroupSelectResultMapping[F]
          with TargetSelectResultMapping[F]
          with TimeSpanMapping[F]
          with TimingWindowMappings[F]
          with UpdateAsterismsResultMapping[F]
          with UpdateGroupsResultMapping[F]
          with UpdateObsAttachmentsResultMapping[F]
          with UpdateObservationsResultMapping[F]
          with UpdateProgramsResultMapping[F]
          with UpdateProposalAttachmentsResultMapping[F]
          with UpdateTargetsResultMapping[F]
          with UserMapping[F]
          with WavelengthMapping[F]
        {

          // Our schema
          val schema: Schema =
            unsafeLoadSchema("OdbSchema.graphql") |+| enums.schema

          // Our services and resources needed by various mappings.
          override val commitHash = commitHash0
          override val itcClient = itcClient0
          override val user: User = user0
          override val topics: Topics[F] = topics0
          override val services: Resource[F, Services[F]] = pool.map(Services.forUser(user))
          override val plannedTimeCalculator: PlannedTimeCalculator.ForInstrumentMode = ptc
          override val httpClient: Client[F] = httpClient0

          // Our combined type mappings
          override val typeMappings: List[TypeMapping] =
            List(
              AddConditionsEntryResultMapping,
              AddDatasetEventResultMapping,
              AddSequenceEventResultMapping,
              AddStepEventResultMapping,
              AirMassRangeMapping,
              AllocationMapping,
              AngleMapping,
              AsterismGroupMapping,
              AsterismGroupSelectResultMapping,
              CatalogInfoMapping,
              CloneObservationResultMapping,
              CloneTargetResultMapping,
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
              DatasetMapping,
              DatasetEventMapping,
              DatasetSelectResultMapping,
              DeclinationMapping,
              ElevationRangeMapping,
              ExecutionMapping,
              FilterTypeMetaMapping,
              GmosCcdModeMapping,
              GmosCustomMaskMapping,
              GmosNorthAtomRecordMapping,
              GmosNorthDynamicMapping,
              GmosNorthFpuMapping,
              GmosNorthGratingConfigMapping,
              GmosNorthLongSlitMapping,
              GmosNorthStaticMapping,
              GmosNorthStepRecordMapping,
              GmosNorthVisitMapping,
              GmosSouthAtomRecordMapping,
              GmosSouthDynamicMapping,
              GmosSouthFpuMapping,
              GmosSouthGratingConfigMapping,
              GmosSouthLongSlitMapping,
              GmosSouthStaticMapping,
              GmosSouthStepRecordMapping,
              GmosSouthVisitMapping,
              GroupMapping,
              GroupEditMapping,
              GroupElementMapping,
              HourAngleRangeMapping,
              LinkUserResultMapping,
              MutationMapping,
              NonsiderealMapping,
              ObsAttachmentMapping,
              ObsAttachmentFileExtMapping,
              ObsAttachmentTypeMetaMapping,
              ObservationEditMapping,
              ObservationMapping,
              ObservingModeMapping,
              ObservationSelectResultMapping,
              OffsetMapping,
              OffsetPMapping,
              OffsetQMapping,
              ParallaxMapping,
              PartnerMetaMapping,
              PartnerSplitMapping,
              PlannedTimeSummaryMapping,
              PosAngleConstraintMapping,
              ProgramMapping,
              ProgramEditMapping,
              ProgramSelectResultMapping,
              ProgramUserMapping,
              ProperMotionDeclinationMapping,
              ProperMotionMapping,
              ProperMotionRaMapping,
              ProposalAttachmentMapping,
              ProposalAttachmentTypeMetaMapping,
              ProposalMapping,
              QueryMapping,
              RadialVelocityMapping,
              RecordDatasetResultMapping,
              RecordGmosNorthAtomResultMapping,
              RecordGmosNorthStepResultMapping,
              RecordGmosNorthVisitResultMapping,
              RecordGmosSouthAtomResultMapping,
              RecordGmosSouthStepResultMapping,
              RecordGmosSouthVisitResultMapping,
              RightAscensionMapping,
              ScienceRequirementsMapping,
              SequenceEventMapping,
              SpectroscopyScienceRequirementsMapping,
              SetAllocationResultMapping,
              SiderealMapping,
              StepConfigMapping,
              StepConfigBiasMapping,
              StepConfigDarkMapping,
              StepConfigGcalMapping,
              StepConfigScienceMapping,
              StepConfigSmartGcalMapping,
              StepEventMapping,
              SubscriptionMapping,
              TargetEditMapping,
              TargetEnvironmentMapping,
              TargetGroupMapping,
              TargetGroupSelectResultMapping,
              TargetMapping,
              TargetSelectResultMapping,
              TimingWindowEndAfterMapping,
              TimingWindowEndAtMapping,
              TimingWindowEndMapping,
              TimingWindowMapping,
              TimingWindowRepeatMapping,
              TimeSpanMapping,
              UpdateAsterismsResultMapping,
              UpdateGroupsResultMapping,
              UpdateObsAttachmentsResultMapping,
              UpdateObservationsResultMapping,
              UpdateProgramsResultMapping,
              UpdateProposalAttachmentsResultMapping,
              UpdateTargetsResultMapping,
              UserMapping,
              WavelengthMapping
            ) ++ LeafMappings ++ ProposalClassMappings

          // Our combined select elaborator
          override val selectElaborator: SelectElaborator =
            SelectElaborator(
              List(
                AsterismGroupElaborator,
                ConstraintSetGroupElaborator,
                ExecutionElaborator,
                GroupElaborator,
                MutationElaborator,
                ObservationElaborator,
                ProgramElaborator,
                SubscriptionElaborator,
                TargetEnvironmentElaborator,
                TargetGroupElaborator,
                QueryElaborator,
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
          QueryMapping,
        ) ++ LeafMappings
    
    }
        
}
