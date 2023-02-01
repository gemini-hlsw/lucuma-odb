// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import _root_.skunk.AppliedFragment
import _root_.skunk.Session
import cats.Applicative
import cats.ApplicativeError
import cats.Monoid
import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.data.ValidatedNel
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
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosDouble
import fs2.Stream
import fs2.concurrent.Topic
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.itc.client.ItcClient
import lucuma.itc.client.ItcResult
import lucuma.itc.client.SpectroscopyModeInput
import lucuma.itc.client.SpectroscopyResult
import lucuma.odb.graphql._
import lucuma.odb.graphql.client.Itc
import lucuma.odb.graphql.enums.FilterTypeEnumType
import lucuma.odb.graphql.enums.PartnerEnumType
import lucuma.odb.graphql.instances.ItcResultEncoder.given
import lucuma.odb.graphql.mapping.UpdateObservationsResultMapping
import lucuma.odb.graphql.mapping._
import lucuma.odb.graphql.topic.ObservationTopic
import lucuma.odb.graphql.topic.ProgramTopic
import lucuma.odb.graphql.topic.TargetTopic
import lucuma.odb.graphql.util._
import lucuma.odb.service.AllocationService
import lucuma.odb.service.AsterismService
import lucuma.odb.service.ItcInputService
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
    observation: Topic[F, ObservationTopic.Element],
    target:      Topic[F, TargetTopic.Element],
  )

  object Topics {
    def apply[F[_]: Concurrent: Logger](pool: Resource[F, Session[F]]): Resource[F, Topics[F]] =
      for {
        sup <- Supervisor[F]
        ses <- pool
        pro <- Resource.eval(ProgramTopic(ses, 1024, sup))
        obs <- Resource.eval(ObservationTopic(ses, 1024, sup))
        tar <- Resource.eval(TargetTopic(ses, 1024, sup))
      } yield Topics(pro, obs, tar)
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
    database:  Resource[F, Session[F]],
    monitor:   SkunkMonitor[F],
    user0:     User,
    topics0:   Topics[F],
    itcClient: ItcClient[F]
  ):  F[Mapping[F]] =
    Trace[F].span(s"Creating mapping for ${user0.displayName} (${user0.id}, ${user0.role})") {
      database.use(enumSchema(_)).map { enums =>
        new SkunkMapping[F](database, monitor) with BaseMapping[F]
          with AirMassRangeMapping[F]
          with AllocationMapping[F]
          with AngleMapping[F]
          with AsterismGroupMapping[F]
          with AsterismGroupSelectResultMapping[F]
          with CatalogInfoMapping[F]
          with CloneTargetResultMapping[F]
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
          with TargetEditMapping[F]
          with TargetEnvironmentMapping[F]
          with TargetMapping[F]
          with TargetGroupMapping[F]
          with TargetGroupSelectResultMapping[F]
          with TargetSelectResultMapping[F]
          with TimeSpanMapping[F]
          with UpdateAsterismsResultMapping[F]
          with UpdateObservationsResultMapping[F]
          with UpdateProgramsResultMapping[F]
          with UpdateTargetsResultMapping[F]
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

          override val itcClientService: Resource[F, ItcInputService[F]] =
            pool.map { s =>
              val oms = ObservingModeServices.fromSession(s)
              ItcInputService.fromSession(s, user, oms)
            }

          override def itcQuery(
            path:     Path,
            pid:      Program.Id,
            oid:      Observation.Id,
            useCache: Boolean
          ): F[Json] = {

            import lucuma.odb.graphql.instances.ItcResultEncoder._

            Itc
              .fromClientAndService(itcClient, itcClientService)
              .queryOne(pid, oid, useCache)
              .map(_.asJson)
          }

          override def sequence(
            path:     Path,
            pid:      Program.Id,
            oid:      Observation.Id,
            useCache: Boolean
          ): F[Json] = {

            import lucuma.odb.graphql.instances.ItcResultEncoder.*
            import lucuma.odb.json.sequence.given

            Itc
              .fromClientAndService(itcClient, itcClientService)
              .queryOne(pid, oid, useCache)
              .map {
                case None     =>
                  Json.Null
                case Some(rs) =>
                  val common = List(
                    "programId"     -> pid.asJson,
                    "observationId" -> oid.asJson,
                    "itcResult"     -> rs.asJson
                  )
                  val fields = rs.value.focus match {
                    case Itc.Result.Missing(_, params)                 =>
                      List(
                        "message" -> s"Could not generate the sequence, missing parameters: ${params.map(_.stringValue).intercalate(", ")}".asJson
                      )

                    case Itc.Result.ServiceError(_, _, msg)            =>
                      List(
                        "message" -> s"Could not generate the sequence, ITC service error: $msg".asJson
                      )

                    case Itc.Result.Success(tid, in, time, count, s2n) if count.value <= 0 =>
                      List(
                        "message" -> s"The ITC service indicates that zero exposures are needed.".asJson
                      )

                    case Itc.Result.Success(tid, in, time, count, s2n) =>
                      import cats.data.NonEmptyList
                      import java.util.UUID
                      import lucuma.core.enums.Breakpoint
                      import lucuma.core.enums.MosPreImaging.IsNotMosPreImaging
                      import lucuma.core.enums.GmosNorthDetector.{Hamamatsu => HamamatsuNorth}
                      import lucuma.core.enums.GmosNorthFilter
                      import lucuma.core.enums.GmosNorthStageMode.FollowXy
                      import lucuma.core.enums.GmosSouthDetector.{Hamamatsu => HamamatsuSouth}
                      import lucuma.core.enums.GmosSouthFilter
                      import lucuma.core.enums.GmosSouthStageMode.FollowXyz
                      import lucuma.core.model.sequence.Atom
                      import lucuma.core.model.sequence.DynamicConfig
                      import lucuma.core.model.sequence.ExecutionSequence
                      import lucuma.core.model.sequence.FutureExecutionConfig
                      import lucuma.core.model.sequence.StaticConfig
                      import lucuma.core.model.sequence.Step
                      import lucuma.core.model.sequence.StepTime
                      import lucuma.core.syntax.timespan.*
                      import lucuma.core.util.TimeSpan
                      import lucuma.itc.client.InstrumentMode
                      import lucuma.itc.client.GmosFpu
                      import lucuma.odb.sequence.data.AcqExposureTime
                      import lucuma.odb.sequence.data.ProtoStep
                      import lucuma.odb.sequence.data.SciExposureTime
                      import lucuma.odb.sequence.gmos.longslit.Acquisition
                      import lucuma.odb.sequence.gmos.longslit.Config
                      import lucuma.odb.sequence.gmos.longslit.Science

                      import lucuma.odb.json.all.query.given

                      val ex: Either[String, FutureExecutionConfig] = in.mode match {
                        case InstrumentMode.GmosNorth(g, f, GmosFpu.North(Right(u))) =>

                          val acq =
                            Acquisition
                              .GmosNorth
                              .compute(
                                // TODO: replace with GmosNorthFilter.acquisition
                                NonEmptyList.of(GmosNorthFilter.UPrime, GmosNorthFilter.GPrime, GmosNorthFilter.RPrime, GmosNorthFilter.IPrime, GmosNorthFilter.ZPrime),
                                u,
                                AcqExposureTime(10.secTimeSpan),
                                in.wavelength
                              )

                          // TODO: i think spectroscopy mode input needs to be generalized to get the whole observing mode

                          val sci =
                            Science
                              .GmosNorth
                              .compute(
                                Config.GmosNorth(g, f, u, in.wavelength), // TODO: okay, we need to read this since the defaults may be overridden (see TODO above)
                                SciExposureTime(time),
                                in.sourceProfile,
                                in.constraints.imageQuality,
                                PosDouble.unsafeFrom(2.0)
                              )

                          def atom(ps: ProtoStep[DynamicConfig.GmosNorth]*): Atom.GmosNorth =
                            Atom.GmosNorth(
                              Atom.Id.fromUuid(UUID.randomUUID),  // TODO: I know, I know
                              ps.toList.map(step)
                            )

                          def step(p: ProtoStep[DynamicConfig.GmosNorth]): Step.GmosNorth =
                            Step.GmosNorth(
                              Step.Id.fromUuid(UUID.randomUUID),  // TODO: yeah
                              p.instrumentConfig,
                              p.stepConfig,
                              StepTime(TimeSpan.Min, TimeSpan.Min, TimeSpan.Min, TimeSpan.Min, TimeSpan.Min),
                              Breakpoint.Disabled
                            )

                          FutureExecutionConfig.GmosNorth(
                            StaticConfig.GmosNorth(
                              FollowXy,
                              HamamatsuNorth,
                              IsNotMosPreImaging,
                              None
                            ),
                            ExecutionSequence.GmosNorth(
                              atom(acq.ccd2, acq.p10, acq.slit),
                              Nil
                            ),
                            ExecutionSequence.GmosNorth(
                              atom(sci.head.steps.toList*),
                              sci.tail.take(count.value-1).toList.map { a =>
                                atom(a.steps.toList*)
                              }
                            )
                          ).asRight[String]

                        case InstrumentMode.GmosNorth(_, _, GmosFpu.North(Left(_))) =>
                          "Cannot generate a GMOS North sequence with a custom FPU".asLeft[FutureExecutionConfig]

                        case InstrumentMode.GmosSouth(g, f, GmosFpu.South(Right(u))) =>

                          val acq =
                            Acquisition
                              .GmosSouth
                              .compute(
                                // TODO: replace with GmosNorthFilter.acquisition
                                NonEmptyList.of(GmosSouthFilter.UPrime, GmosSouthFilter.GPrime, GmosSouthFilter.RPrime, GmosSouthFilter.IPrime, GmosSouthFilter.ZPrime),
                                u,
                                AcqExposureTime(10.secTimeSpan),
                                in.wavelength
                              )

                          // TODO: i think spectroscopy mode input needs to be generalized to get the whole observing mode

                          val sci =
                            Science
                              .GmosSouth
                              .compute(
                                Config.GmosSouth(g, f, u, in.wavelength), // TODO: okay, we need to read this since the defaults may be overridden (see TODO above)
                                SciExposureTime(time),
                                in.sourceProfile,
                                in.constraints.imageQuality,
                                PosDouble.unsafeFrom(2.0)
                              )

                          def atom(ps: ProtoStep[DynamicConfig.GmosSouth]*): Atom.GmosSouth =
                            Atom.GmosSouth(
                              Atom.Id.fromUuid(UUID.randomUUID),  // TODO: I know, I know
                              ps.toList.map(step)
                            )

                          def step(p: ProtoStep[DynamicConfig.GmosSouth]): Step.GmosSouth =
                            Step.GmosSouth(
                              Step.Id.fromUuid(UUID.randomUUID),  // TODO: yeah
                              p.instrumentConfig,
                              p.stepConfig,
                              StepTime(TimeSpan.Min, TimeSpan.Min, TimeSpan.Min, TimeSpan.Min, TimeSpan.Min),
                              Breakpoint.Disabled
                            )

                          FutureExecutionConfig.GmosSouth(
                            StaticConfig.GmosSouth(
                              FollowXyz,
                              HamamatsuSouth,
                              IsNotMosPreImaging,
                              None
                            ),
                            ExecutionSequence.GmosSouth(
                              atom(acq.ccd2, acq.p10, acq.slit),
                              Nil
                            ),
                            ExecutionSequence.GmosSouth(
                              atom(sci.head.steps.toList*),
                              sci.tail.take(count.value-1).toList.map { a =>
                                atom(a.steps.toList*)
                              }
                            )
                          ).asRight[String]

                        case InstrumentMode.GmosSouth(_, _, GmosFpu.South(Left(_))) =>
                          "Cannot generate a GMOS South sequence with a custom FPU".asLeft[FutureExecutionConfig]

                        case _ =>
                          "Cannot generate a sequence for this observation's observing mode".asLeft[FutureExecutionConfig]
                      }

                      List(
                        ex match {
                          case Left(msg) =>
                            "message" -> msg.asJson
                          case Right(f) =>
                            "executionConfig" -> f.asJson
                        }
                      )
                  }
                  Json.fromFields(common ::: fields)
              }
          }

          // Our combined type mappings
          override val typeMappings: List[TypeMapping] =
            List(
              AirMassRangeMapping,
              AllocationMapping,
              AngleMapping,
              AsterismGroupMapping,
              AsterismGroupSelectResultMapping,
              CatalogInfoMapping,
              CloneTargetResultMapping,
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
              TargetEditMapping,
              TargetEnvironmentMapping,
              TargetGroupMapping,
              TargetGroupSelectResultMapping,
              TargetMapping,
              TargetSelectResultMapping,
              TimeSpanMapping,
              UpdateAsterismsResultMapping,
              UpdateObservationsResultMapping,
              UpdateProgramsResultMapping,
              UpdateTargetsResultMapping,
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