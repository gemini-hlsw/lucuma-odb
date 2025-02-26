// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.data.Ior
import cats.data.NonEmptyChain
import cats.effect.*
import cats.effect.std.Supervisor
import cats.effect.std.UUIDGen
import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.IORuntimeConfig
import cats.implicits.*
import clue.FetchClient
import clue.GraphQLOperation
import clue.ResponseException
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import clue.http4s.Http4sWebSocketBackend
import clue.http4s.Http4sWebSocketClient
import clue.model.GraphQLErrors
import clue.websocket.WebSocketClient
import com.dimafeng.testcontainers.PostgreSQLContainer
import com.dimafeng.testcontainers.munit.TestContainerForAll
import eu.timepit.refined.types.numeric.NonNegInt
import fs2.Stream
import fs2.text.utf8
import grackle.Mapping
import grackle.skunk.SkunkMonitor
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.syntax.*
import io.laserdisc.pure.s3.tagless.S3AsyncClientOp
import lucuma.core.data.EmailAddress
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcVersions
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.TargetIntegrationTimeOutcome
import lucuma.itc.client.ImagingIntegrationTimeInput
import lucuma.itc.client.IntegrationTimeResult
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.odb.Config
import lucuma.odb.FMain
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.AttachmentFileService.AttachmentException
import lucuma.odb.service.S3FileService
import lucuma.refined.*
import munit.CatsEffectSuite
import munit.internal.console.AnsiColors
import natchez.Trace.Implicits.noop
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.client.Client
import org.http4s.headers.Authorization
import org.http4s.jdkhttpclient.JdkHttpClient
import org.http4s.jdkhttpclient.JdkWSClient
import org.http4s.server.Server
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.{Uri as Http4sUri, *}
import org.slf4j
import org.testcontainers.containers.BindMode
import org.testcontainers.containers.PostgreSQLContainer.POSTGRESQL_PORT
import org.testcontainers.utility.DockerImageName
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import skunk.Session
import software.amazon.awssdk.services.s3.model.S3Exception
import software.amazon.awssdk.services.s3.presigner.S3Presigner

import java.net.SocketException
import scala.concurrent.duration.*

object OdbSuite:
  def reportFailure: Throwable => Unit =
    case e: IllegalArgumentException if e.getMessage == "statusCode" => () // swallow annoying error ... not sure where it comes from though ... :-\
    case _: AttachmentException => ()
    case _: S3Exception => ()
    case e: SocketException if e.getMessage == "Connection reset" => ()
    case e => print("OdbSuite.reportFailure: "); e.printStackTrace

  // a runtime that is constructed the same as global, but lets us see unhandled errors (above)
  val runtime: IORuntime =
    val (compute, _) = IORuntime.createWorkStealingComputeThreadPool(reportFailure = reportFailure)
    val (blocking, _) = IORuntime.createDefaultBlockingExecutionContext()
    val (scheduler, _) = IORuntime.createDefaultScheduler()
    IORuntime(compute, blocking, scheduler, () => (), IORuntimeConfig())

/**
 * Mixin that allows execution of GraphQL operations on a per-suite instance of the Odb, shared
 * among all tests.
 */
abstract class OdbSuite(debug: Boolean = false) extends CatsEffectSuite with TestContainerForAll with DatabaseOperations with TestSsoClient with ChronicleOperations {
  override implicit def munitIoRuntime: IORuntime = OdbSuite.runtime

  /** Ensure that exactly the specified errors are reported, in order. */
  def interceptGraphQL(messages: String*)(fa: IO[Any]): IO[Unit] =
    fa.attempt.flatMap {
      case Left(ResponseException(errors, _)) =>
        assertEquals(messages.toList, errors.toList.map(_.message)).pure[IO]
      case Left(other) => IO.raiseError(other)
      case Right(a) => fail(s"Expected failure, got $a")
    }

  /** Intercept a single OdbError */
  def interceptOdbError(fa: IO[Any])(f: PartialFunction[OdbError, Unit]): IO[Unit] =
    fa.attempt.flatMap {
      case Left(e @ ResponseException(errors, _)) =>
        errors.toList.flatMap(OdbError.fromGraphQLError) match
          case List(odbe) =>
            IO(f.applyOrElse(odbe, e => fail(s"OdbError predicate failed on $e")))
          case _ => IO.raiseError(e)
      case Left(other) => IO.raiseError(other)
      case Right(a) => fail(s"Expected failure, got $a")
    }

  private val it = Iterator.from(1)

  /** Generate a new id, impurely. */
  def nextId: Long = it.next().toLong

  val jlogger: slf4j.Logger =
    slf4j.LoggerFactory.getLogger("lucuma-odb-test-container")

  var container: PostgreSQLContainer = null

  override val containerDef = new PostgreSQLContainer.Def(DockerImageName.parse("postgres:15")) {
    override def createContainer(): PostgreSQLContainer = {
      val c = super.createContainer()
      c.container.withClasspathResourceMapping("/db/migration", "/docker-entrypoint-initdb.d/", BindMode.READ_ONLY)
      if (debug) {
        c.container.withLogConsumer{f =>
          jlogger.debug(s"${AnsiColors.CYAN}${f.getUtf8String().trim()}${AnsiColors.Reset}")
        }
        ()
      }
      container = c
      c
    }
  }

  implicit val log: Logger[IO] =
    Slf4jLogger.getLoggerFromName("lucuma-odb-test")

  val FakeItcVersions: ItcVersions =
    ItcVersions("foo", "bar".some)

  val FakeBandOrLine: Either[Band, Wavelength] =
    Band.B.asLeft

  val FakeItcResult: IntegrationTime =
    IntegrationTime(
      10.secTimeSpan,
      NonNegInt.unsafeFrom(6),
      SignalToNoise.unsafeFromBigDecimalExact(50.0)
    )

  // Provides a hook to allow test cases to alter the dummy ITC results.
  def fakeItcImagingResult: IntegrationTime =
    FakeItcResult

  def fakeItcSpectroscopyResult: IntegrationTime =
    FakeItcResult

  protected def itcClient: ItcClient[IO] =
    new ItcClient[IO] {

      override def imaging(input: ImagingIntegrationTimeInput, useCache: Boolean): IO[IntegrationTimeResult] =
        IntegrationTimeResult(
          FakeItcVersions,
          AsterismIntegrationTimeOutcomes(
            NonEmptyChain.fromSeq(
              List.fill(input.asterism.length)(
                TargetIntegrationTimeOutcome(
                  TargetIntegrationTime(Zipper.one(fakeItcImagingResult), FakeBandOrLine).asRight
                )
              )
            ).get
          )
        ).pure[IO]

      override def spectroscopy(input: SpectroscopyIntegrationTimeInput, useCache: Boolean): IO[IntegrationTimeResult] = {
        val signal = lucuma.core.math.Wavelength.fromIntNanometers(666).get
        val wavelength = input.mode match
          case lucuma.itc.client.InstrumentMode.GmosNorthSpectroscopy(w, _, _, _, _, _) => w
          case lucuma.itc.client.InstrumentMode.GmosSouthSpectroscopy(w, _, _, _, _, _) => w
          case _                                                                        => signal
        IO.whenA(wavelength === signal) {
          IO.raiseError(new RuntimeException("Artifical exception for test cases."))
        } *> {
          val result =
            input.exposureTimeMode match
              case ExposureTimeMode.SignalToNoiseMode(_, _)   =>
                fakeItcSpectroscopyResult
              case ExposureTimeMode.TimeAndCountMode(t, c, _) =>
                IntegrationTime(t, c, fakeItcSpectroscopyResult.signalToNoise)

           IntegrationTimeResult(
            FakeItcVersions,
            AsterismIntegrationTimeOutcomes(
              NonEmptyChain.fromSeq(
                List.fill(input.asterism.length)(
                  TargetIntegrationTimeOutcome(
                    TargetIntegrationTime(Zipper.one(result), FakeBandOrLine).asRight
                  )
                )
              ).get
            )
          ).pure[IO]
        }
      }

      def spectroscopyGraphs(
        input: lucuma.itc.client.SpectroscopyGraphsInput,
        useCache: Boolean
      ): IO[lucuma.itc.client.SpectroscopyGraphsResult] =
        IO.raiseError(new java.lang.RuntimeException("spectroscopyGraph: not implemented"))

      def spectroscopyIntegrationTimeAndGraphs(
        input:    lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsInput,
        useCache: Boolean = true
      ): IO[lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsResult] =
        IO.raiseError(new java.lang.RuntimeException("spectroscopyIntegrationTimeAndGraph: not implemented"))

      override def versions: IO[ItcVersions] =
        FakeItcVersions.pure[IO]
    }

  // override in tests that need an http client
  protected def httpRequestHandler: Request[IO] => Resource[IO, Response[IO]] = _ => Resource.eval(IO.pure(Response.notFound[IO]))

  // tests that require successfully sending invitations can assign this to httpRequestHandler
  protected val invitationEmailRequestHandler: Request[IO] => Resource[IO, Response[IO]] =
    req => {
      val sio = UUIDGen[IO].randomUUID.map(uuid =>
        Json.obj(
          "id"      -> s"<$uuid>".asJson,
          "message" -> "Queued".asJson
        ).toString
      )
      Resource.eval(IO.pure(Response(body = Stream.eval(sio).through(utf8.encode))))
    }

  private def httpClient: Client[IO] = Client.apply(httpRequestHandler)

  protected def databaseConfig: Config.Database =
    Config.Database(
      maxConnections = 10,
      maxCalibrationConnections = 10,
      host     = container.containerIpAddress,
      port     = container.mappedPort(POSTGRESQL_PORT),
      user     = container.username,
      password = container.password,
      database = container.databaseName,
    )

  // overriden in OdbSuiteWithS3 for tests that need it.
  protected def awsConfig: Config.Aws =
    Config.Aws(
      accessKey       = "accessKey".refined,
      secretKey       = "secretkey".refined,
      basePath        = "basePath".refined,
      bucketName      = fs2.aws.s3.models.Models.BucketName("bucketName".refined),
      fileUploadMaxMb = 5
    )

  protected def emailConfig: Config.Email =
    Config.Email(
      apiKey            = "apiKey".refined,
      domain            = "gpp.com".refined,
      webhookSigningKey = "webhookKey".refined,
      invitationFrom    = EmailAddress.unsafeFrom("explore@gpp.com"),
      exploreUrl = Http4sUri.fromString("https://nonsense.kom").toOption.get
    )

  // These are overriden in OdbSuiteWithS3 for tests that need it.
  protected def s3ClientOpsResource: Resource[IO, S3AsyncClientOp[IO]] =
    S3FileService.s3AsyncClientOpsResource[IO](awsConfig)

  protected def s3PresignerResource: Resource[IO, S3Presigner] =
    S3FileService.s3PresignerResource[IO](awsConfig)

  protected def goaUsers: Set[User.Id] =
    Set.empty

  private def httpApp: Resource[IO, WebSocketBuilder2[IO] => HttpApp[IO]] =
    FMain.routesResource[IO](
      databaseConfig,
      awsConfig,
      emailConfig,
      itcClient.pure[Resource[IO, *]],
      CommitHash.Zero,
      goaUsers,
      ssoClient.pure[Resource[IO, *]],
      true,
      List("unused"),
      s3ClientOpsResource,
      s3PresignerResource,
      httpClient.pure[Resource[IO, *]]
    ).map(_.map(_.orNotFound))

  /** Resource yielding an instantiated OdbMapping, which we can use for some whitebox testing. */
  def mapping: Resource[IO, Mapping[IO]] =
    for {
      db  <- FMain.databasePoolResource[IO](databaseConfig)
      mon  = SkunkMonitor.noopMonitor[IO]
      usr  = TestUsers.Standard.pi(11, 110)
      top <- OdbMapping.Topics(db)
      itc  = itcClient
      enm <- db.evalMap(Enums.load)
      ptc <- db.evalMap(TimeEstimateCalculatorImplementation.fromSession(_, enm))
      map  = OdbMapping(db, mon, usr, top, itc, CommitHash.Zero, goaUsers, enm, ptc, httpClient, emailConfig)
    } yield map

  protected def server: Resource[IO, Server] =
    for {
      a <- httpApp
      s <- BlazeServerBuilder[IO]
             .withHttpWebSocketApp(a)
             .bindAny()
             .resource
    } yield s

  protected def session: Resource[IO, Session[IO]] =
    FMain.singleSession(databaseConfig)

  private def transactionalClient(user: User)(svr: Server): IO[FetchClient[IO, Nothing]] =
    authorizationHeader(user).flatMap: auth =>
      for {
        xbe <- JdkHttpClient.simple[IO].map(Http4sHttpBackend[IO](_))
        uri  = svr.baseUri / "odb"
        xc  <- Http4sHttpClient.of[IO, Nothing](uri, headers = Headers(auth))(Async[IO], xbe, Logger[IO])
      } yield xc

  private def streamingClient(user: User)(svr: Server): Resource[IO, WebSocketClient[IO, Nothing]] =
    Resource.eval(authorizationObject(user)).flatMap: ps =>
      for {
        sbe <- Resource.eval(JdkWSClient.simple[IO].map(Http4sWebSocketBackend[IO](_)))
        uri  = (svr.baseUri / "ws").copy(scheme = Some(Http4sUri.Scheme.unsafeFromString("ws")))
        sc  <- Resource.eval(Http4sWebSocketClient.of[IO, Nothing](uri)(using Async[IO], Logger[IO], sbe))
        _   <- Resource.make(sc.connect(ps.pure[IO]))(_ => sc.disconnect())
      } yield sc

  case class Operation(document: String) extends GraphQLOperation.Typed[Nothing, JsonObject, Json]

  private lazy val serverFixture: Fixture[Server] =
    ResourceSuiteLocalFixture("server", server)

  private lazy val sessionFixture: Fixture[Session[IO]] =
    ResourceSuiteLocalFixture("session", session)

  override def munitFixtures = List(serverFixture, sessionFixture)

  sealed trait ClientOption extends Product with Serializable {

    def prefix: String =
      this match {
        case ClientOption.Http => "http     "
        case ClientOption.Ws   => "websocket"
      }

    def connection(user: User): Server => Resource[IO, FetchClient[IO, Nothing]] =
      this match {
        case ClientOption.Http => s => Resource.eval(transactionalClient(user)(s))
        case ClientOption.Ws   => streamingClient(user)
      }

  }

  object ClientOption {
    case object Http extends ClientOption
    case object Ws   extends ClientOption

    val All: List[ClientOption] = List(Http, Ws)

    val Default: ClientOption = Http
  }

  override def beforeAll(): Unit = {
    super.beforeAll()

    dbInitialization.foreach { init =>
      FMain
        .databasePoolResource[IO](databaseConfig)
        .flatten
        .use(init)
        .unsafeRunSync()
    }

    startS3
  }

  override def afterAll(): Unit = {
    super.afterAll()
    stopS3
  }

  /**
   * Perform any database initialization required by the test suite.
   *
   * @return database initialization function wrapped in an Option; None if
   *         there is no required initialization
   */
  def dbInitialization: Option[Session[IO] => IO[Unit]] =
    None

  // Override in tests that need S3.
  // For some reason, overriding beforeAll and afterAll in OdbSuiteWithS3 didn't work.
  protected def startS3: Unit = ()
  protected def stopS3: Unit = ()

  def expect(
    user:      User,
    query:     String,
    expected:  Either[List[String], Json],
    variables: Option[JsonObject] = None,
    client:    ClientOption = ClientOption.Default
  ): IO[Unit] = {
    val op = this.query(user, query, variables, client)
    expected.fold(errors => {
      op
      .intercept[ResponseException[Any]]
        .map { e => e.errors.toList.map(_.message) }
        .assertEquals(errors)
    }, success => {
      op.map(_.spaces2)
//        .flatTap(s => IO.println(s))
        .assertEquals(success.spaces2) // by comparing strings we get more useful errors
    })
  }

  /** Expect success. */
  def expectSuccess(
    user:      User,
    query:     String,
    expected:  Json,
    variables: Option[JsonObject] = None,
    client:    ClientOption = ClientOption.Default
  ): IO[Unit] =
    this.query(user, query, variables, client)
      .map(_.spaces2)
      .assertEquals(expected.spaces2) // by comparing strings we get more useful errors

  /** Expect a single OdbError */
  def expectOdbError(
    user:      User,
    query:     String,
    expected:  PartialFunction[OdbError, Unit],
    variables: Option[JsonObject] = None,
    client:    ClientOption = ClientOption.Default
  ): IO[Unit] =
    this.query(user, query, variables, client)
      .intercept[ResponseException[Any]]
      .map: er =>
        er.errors
          .toList
          .map(OdbError.fromGraphQLError(_)) // List[Option[OdbError]]
          .foreach:
            case None    => fail("Received a non-odb error.")
            case Some(e) =>
              expected.lift(e) match
                case None => fail(s"Unexpected ODB error: $e")
                case Some(_) => () // ok

  def expectOdbErrors(
    user:      User,
    query:     String,
    expected:  Set[OdbError],
    variables: Option[JsonObject] = None,
    client:    ClientOption = ClientOption.Default
  ): IO[Unit] =
    this.query(user, query, variables, client)
      .intercept[ResponseException[Any]]
      .map: e =>
        assertEquals(e.errors.toList.flatMap(OdbError.fromGraphQLError(_).toList).toSet, expected)

  def expectSuccessOrOdbError(
    user:      User,
    query:     String,
    expected:  Either[PartialFunction[OdbError, Unit], Json],
    variables: Option[JsonObject] = None,
    client:    ClientOption = ClientOption.Default
  ): IO[Unit] =
    expected.fold(
      expectOdbError(user, query, _, variables, client),
      expectSuccess(user, query, _, variables, client)
    )

  def expectIor(
    user:      User,
    query:     String,
    expected:  Ior[List[String], Json],
    variables: Option[JsonObject] = None,
    client:    ClientOption = ClientOption.Default
  ): IO[Unit] =
    queryIor(user, query, variables, client).map: ior =>
      assertEquals(ior.leftMap(_.toList.map(_.message)), expected)
        
  def query(
    user:      User,
    query:     String,
    variables: Option[JsonObject] = None,
    client:    ClientOption = ClientOption.Default
  ): IO[Json] =
    Resource.eval(IO(serverFixture()))
      .flatMap(client.connection(user))
      .use { conn =>
//        println(s"[$query]")
        val req = conn.request(Operation(query))
        val op  = variables.fold(req.apply)(req.withInput).raiseGraphQLErrors
        op.onError:
          case ResponseException(es, _) =>
            es.traverse_ : e =>
              OdbError.fromGraphQLError(e) match
                case Some(_) => IO.unit
                case None => IO.println(s"🐙 Not an OdbError: $e")
          case _ => IO.unit
      }

  def queryIor(
    user:      User,
    query:     String,
    variables: Option[JsonObject] = None,
    client:    ClientOption = ClientOption.Default
  ): IO[Ior[GraphQLErrors, Json]] =
    Resource.eval(IO(serverFixture()))
      .flatMap(client.connection(user))
      .use { conn =>
        val req = conn.request(Operation(query))
        val op  = variables.fold(req.apply)(req.withInput)
        op.map(_.result)
      }

  def subscription(user: User, query: String, mutations: Either[List[(String, Option[JsonObject])], IO[Any]], variables: Option[JsonObject] = None): IO[List[Json]] =
    Supervisor[IO].use { sup =>
      Resource.eval(IO(serverFixture()))
        .flatMap(streamingClient(user))
        .use { conn =>
          val req = conn.subscribe(Operation(query))
          variables.fold(req.apply)(req.withInput)
            .logGraphQLErrors(_ => "****** Error in GraphQL Subscription")
            .allocated
            .flatMap { case (sub, cleanup) =>
              for {
                _   <- log.info("*** ----- about to start stream fiber")
                fib <- sup.supervise(sub.compile.toList)
                _   <- log.info("*** ----- pausing a bit")
                _   <- IO.sleep(1.second)
                _   <- log.info("*** ----- running mutations")
                _   <- mutations.fold(_.traverse_ { case (query, vars) =>
                  val req = conn.request(Operation(query))
                  vars.fold(req.apply)(req.withInput)
                }, identity)
                _   <- log.info("*** ----- pausing a bit")
                _   <- IO.sleep(1.second)
                _   <- log.info("*** ----- stopping subscription")
                _   <- cleanup
                _   <- log.info("*** ----- joining fiber")
                obt <- fib.joinWithNever
              } yield obt
            }
        }
    }

  def subscriptionExpect(user: User, query: String, mutations: Either[List[(String, Option[JsonObject])], IO[Any]], expected: List[Json], variables: Option[JsonObject] = None) =
    subscription(user, query, mutations, variables).map { obt =>
      assertEquals(obt.map(_.spaces2), expected.map(_.spaces2))  // by comparing strings we get more useful errors
    }

  def subscriptionExpectF(user: User, query: String, mutations: Either[List[(String, Option[JsonObject])], IO[Any]], expectedF: IO[List[Json]], variables: Option[JsonObject] = None) =
    subscription(user, query, mutations, variables).flatMap { obt =>
      expectedF.map { expected =>
        assertEquals(obt.map(_.spaces2), expected.map(_.spaces2))  // by comparing strings we get more useful errors
      }
    }

  def subscriptionExpectFT(user: User, query: String, mutations: Either[List[(String, Option[JsonObject])], IO[Any]], expectedF: IO[List[Json]], variables: Option[JsonObject] = None, transform: List[Json] => List[Json]) =
    subscription(user, query, mutations, variables).flatMap { obt =>
      expectedF.map { expected =>
        assertEquals(transform(obt).map(_.spaces2), transform(expected).map(_.spaces2))  // by comparing strings we get more useful errors
      }
    }

  def withSession[A](f: Session[IO] => IO[A]): IO[A] =
    Resource.eval(IO(sessionFixture())).use(f)

  import lucuma.odb.service.Services
  def withServices[A](u: User)(f: Services[IO] => IO[A]): IO[A] =
    Resource.eval(IO(sessionFixture())).use { s =>
      Enums.load(s).flatMap(e =>
        f(Services.forUser(u, e, None)(s))
      )
    }

}
