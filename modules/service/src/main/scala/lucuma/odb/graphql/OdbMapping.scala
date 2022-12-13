// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import _root_.skunk.AppliedFragment
import _root_.skunk.Session
import cats.Applicative
import cats.Monoid
import cats.data.NonEmptyList
import cats.effect.std.Supervisor
import cats.effect.{Unique => _, _}
import cats.syntax.all._
import com.github.vertical_blank.sqlformatter.SqlFormatter
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.QueryCompiler.SelectElaborator
import edu.gemini.grackle._
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.skunk.SkunkMonitor
import edu.gemini.grackle.sql.SqlMapping
import fs2.Stream
import fs2.concurrent.Topic
import io.circe.Json
import lucuma.core.model.User
import lucuma.odb.graphql._
import lucuma.odb.graphql.enums.FilterTypeEnumType
import lucuma.odb.graphql.enums.PartnerEnumType
import lucuma.odb.graphql.mapping._
import lucuma.odb.graphql.topic.ObservationTopic
import lucuma.odb.graphql.topic.ProgramTopic
import lucuma.odb.graphql.util._
import lucuma.odb.service.AllocationService
import lucuma.odb.service.AsterismService
import lucuma.odb.service.ItcClientService
import lucuma.odb.service.ObservationService
import lucuma.odb.service.ObservingModeServices
import lucuma.odb.service.ProgramService
import lucuma.odb.service.TargetService
import natchez.Trace
import org.tpolecat.sourcepos.SourcePos
import org.typelevel.log4cats.Logger

import scala.io.AnsiColor
import scala.io.Source

object OdbMapping {

  case class Topics[F[_]](
    program:     Topic[F, ProgramTopic.Element],
    observation: Topic[F, ObservationTopic.Element]
  )

  object Topics {
    def apply[F[_]: Concurrent: Logger](pool: Resource[F, Session[F]]): Resource[F, Topics[F]] =
      for {
        sup <- Supervisor[F]
        ses <- pool
        pro <- Resource.eval(ProgramTopic(ses, 1024, sup))
        obs <- Resource.eval(ObservationTopic(ses, 1024, sup))
      } yield Topics(pro, obs)
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
    database: Resource[F, Session[F]],
    monitor:  SkunkMonitor[F],
    user0:    User,
    topics0:  Topics[F],
  ):  F[Mapping[F]] =
    Trace[F].span(s"Creating mapping for ${user0.displayName} (${user0.id}, ${user0.role})") {
      database.use(enumSchema(_)).map { enums =>
        new SkunkMapping[F](database, monitor) with BaseMapping[F]
          with AirMassRangeMapping[F]
          with AllocationMapping[F]
          with AngleMapping[F]
          with CatalogInfoMapping[F]
          with ConstraintSetGroupMapping[F]
          with ConstraintSetGroupSelectResultMapping[F]
          with ConstraintSetMapping[F]
          with CoordinatesMapping[F]
          with CreateObservationResultMapping[F]
          with CreateProgramResultMapping[F]
          with CreateTargetResultMapping[F]
          with DeclinationMapping[F]
          with ElevationRangeMapping[F]
          with FilterTypeMetaMapping[F]
          with GmosLongSlitMapping[F]
          with HourAngleRangeMapping[F]
          with LeafMappings[F]
          with LinkUserResultMapping[F]
          with MutationMapping[F]
          with NonNegDurationMapping[F]
          with NonsiderealMapping[F]
          with ObservationEditMapping[F]
          with ObservationMapping[F]
          with ObservingModeMapping[F]
          with ObservationSelectResultMapping[F]
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
          with ProperMotionRAMapping[F]
          with ProposalMapping[F]
          with ProposalClassMapping[F]
          with QueryMapping[F]
          with RadialVelocityMapping[F]
          with RightAscensionMapping[F]
          with ScienceRequirementsMapping[F]
          with SetAllocationResultMapping[F]
          with SiderealMapping[F]
          with SpectroscopyScienceRequirementsMapping[F]
          with SubscriptionMapping[F]
          with TargetEnvironmentMapping[F]
          with TargetMapping[F]
          with TargetGroupMapping[F]
          with TargetGroupSelectResultMapping[F]
          with TargetSelectResultMapping[F]
          with UpdateAsterismsResultMapping[F]
          with UpdateObservationsResultMapping[F]
          with UpdateProgramsResultMapping[F]
          with UserMapping[F]
          with WavelengthMapping[F]
        {

          // Our schema
          val schema: Schema =
            unsafeLoadSchema("OdbSchema.graphql") |+| enums

          // Our services and resources needed by various mappings.
          override val user: User         = user0
          override val topics: Topics[F]  = topics0

          override val allocationService: Resource[F, AllocationService[F]] =
            pool.map(AllocationService.fromSessionAndUser(_, user))

          override val asterismService: Resource[F, AsterismService[F]] =
            pool.map(AsterismService.fromSessionAndUser(_, user))

          override val observationService: Resource[F, ObservationService[F]] =
            pool.map { s =>
              val oms = ObservingModeServices.fromSession(s)
              ObservationService.fromSessionAndUser(s, user, oms)
            }

          override val programService: Resource[F, ProgramService[F]] =
            pool.map(ProgramService.fromSessionAndUser(_, user))

          override val targetService: Resource[F, TargetService[F]] =
            pool.map(TargetService.fromSession(_, user))

          override val itcClientService: Resource[F, ItcClientService[F]] =
            pool.map { s =>
              val oms = ObservingModeServices.fromSession(s)
              ItcClientService.fromSession(s, user, oms)
            }

          // Our combined type mappings
          override val typeMappings: List[TypeMapping] =
            List(
              AirMassRangeMapping,
              AllocationMapping,
              AngleMapping,
              CatalogInfoMapping,
              ConstraintSetGroupMapping,
              ConstraintSetGroupSelectResultMapping,
              ConstraintSetMapping,
              CoordinatesMapping,
              CreateObservationResultMapping,
              CreateProgramResultMapping,
              CreateTargetResultMapping,
              DeclinationMapping,
              ElevationRangeMapping,
              FilterTypeMetaMapping,
              GmosNorthLongSlitMapping,
              GmosSouthLongSlitMapping,
              HourAngleRangeMapping,
              LinkUserResultMapping,
              MutationMapping,
              NonNegDurationMapping,
              NonsiderealMapping,
              ObservationEditMapping,
              ObservationMapping,
              ObservingModeMapping,
              ObservationSelectResultMapping,
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
              ProperMotionRAMapping,
              ProposalMapping,
              QueryMapping,
              RadialVelocityMapping,
              RightAscensionMapping,
              ScienceRequirementsMapping,
              SpectroscopyScienceRequirementsMapping,
              SetAllocationResultMapping,
              SiderealMapping,
              SubscriptionMapping,
              TargetEnvironmentMapping,
              TargetGroupMapping,
              TargetGroupSelectResultMapping,
              TargetMapping,
              TargetSelectResultMapping,
              UpdateAsterismsResultMapping,
              UpdateObservationsResultMapping,
              UpdateProgramsResultMapping,
              UserMapping,
              WavelengthMapping
            ) ++ LeafMappings ++ ProposalClassMappings

          // Our combined select elaborator
          override val selectElaborator: SelectElaborator =
            SelectElaborator(
              List(
                ConstraintSetGroupElaborator,
                MutationElaborator,
                ProgramElaborator,
                SubscriptionElaborator,
                TargetEnvironmentElaborator,
                TargetGroupElaborator,
                QueryElaborator,
              ).combineAll
            )

          // Override `defaultRootCursor` to log the GraphQL query. This is optional.
          override def defaultRootCursor(query: Query, tpe: Type, env: Env): F[Result[(Query, Cursor)]] =
            Logger[F].info("\n\n" + PrettyPrinter.query(query).render(100) + "\n") >> 
            super.defaultRootCursor(query, tpe, env)

          // Override `fetch` to log the SQL query. This is optional.
          override def fetch(fragment: AppliedFragment, codecs: List[(Boolean, Codec)]): F[Vector[Array[Any]]] = {
            Logger[F].info {
              val formatted = SqlFormatter.format(fragment.fragment.sql)
              val cleanedUp = formatted.replaceAll("\\$ (\\d+)", "\\$$1") // turn $ 42 into $42
              val colored   = cleanedUp.linesIterator.map(s => s"${AnsiColor.GREEN}$s${AnsiColor.RESET}").mkString("\n")
              s"\n\n$colored\n\n"
            } *>
            super.fetch(fragment, codecs)
          }

        }
      }
    }

  def enumSchema[F[_]: Applicative](s: Session[F]): F[Schema] =
    List(FilterTypeEnumType.fetch(s), PartnerEnumType.fetch(s)).sequence.map { tpes =>
      new Schema {
        def pos: SourcePos = SourcePos.instance
        def types: List[NamedType] = tpes
        def directives: List[Directive] = Nil
      }
    }

}