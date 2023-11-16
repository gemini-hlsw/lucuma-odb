// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.data.Ior
import cats.effect.*
import cats.effect.std.Supervisor
import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.IORuntimeConfig
import cats.implicits.*
import clue.ErrorPolicy
import clue.FetchClient
import clue.GraphQLOperation
import clue.ResponseException
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import clue.http4s.Http4sWebSocketBackend
import clue.http4s.Http4sWebSocketClient
import clue.websocket.WebSocketClient
import com.dimafeng.testcontainers.PostgreSQLContainer
import com.dimafeng.testcontainers.munit.TestContainerForAll
import eu.timepit.refined.types.numeric.PosInt
import grackle.Mapping
import grackle.skunk.SkunkMonitor
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.JsonObject
import io.laserdisc.pure.s3.tagless.S3AsyncClientOp
import lucuma.core.data.Zipper
import lucuma.core.math.SignalToNoise
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.core.util.Gid
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ImagingIntegrationTimeInput
import lucuma.itc.client.IntegrationTimeResult
import lucuma.itc.client.ItcClient
import lucuma.itc.client.ItcVersions
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.odb.Config
import lucuma.odb.FMain
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.logic.TimeEstimateCalculator
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.AttachmentFileService.AttachmentException
import lucuma.odb.service.ProposalService
import lucuma.odb.service.S3FileService
import lucuma.refined.*
import munit.CatsEffectSuite
import munit.internal.console.AnsiColors
import natchez.Trace.Implicits.noop
import org.http4s.Response
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.client.Client
import org.http4s.headers.Authorization
import org.http4s.jdkhttpclient.JdkHttpClient
import org.http4s.jdkhttpclient.JdkWSClient
import org.http4s.server.Server
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.{Uri => Http4sUri, _}
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
    case _: ProposalService.ProposalUpdateException => ()
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
  private implicit val DefaultErrorPolicy: ErrorPolicy.RaiseAlways.type = ErrorPolicy.RaiseAlways

  override implicit def munitIoRuntime: IORuntime = OdbSuite.runtime

  /** Ensure that exactly the specified errors are reported, in order. */
  def interceptGraphQL(messages: String*)(fa: IO[Any]): IO[Unit] =
    fa.attempt.flatMap {
      case Left(ResponseException(errors, _)) =>
        assertEquals(messages.toList, errors.toList.map(_.message)).pure[IO]
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

  val FakeItcResult: IntegrationTime =
    IntegrationTime(
      10.secTimeSpan,
      PosInt.unsafeFrom(6),
      SignalToNoise.unsafeFromBigDecimalExact(50.0)
    )

  private def itcClient: ItcClient[IO] =
    new ItcClient[IO] {

      override def imaging(input: ImagingIntegrationTimeInput, useCache: Boolean): IO[IntegrationTimeResult] =
        IntegrationTimeResult(
          FakeItcVersions,
          Zipper.one(FakeItcResult)
        ).pure[IO]

      override def spectroscopy(input: SpectroscopyIntegrationTimeInput, useCache: Boolean): IO[IntegrationTimeResult] =
        IntegrationTimeResult(
          FakeItcVersions,
          Zipper.one(FakeItcResult)
        ).pure[IO]

      def optimizedSpectroscopyGraph(
        input: lucuma.itc.client.OptimizedSpectroscopyGraphInput,
        useCache: Boolean
      ): IO[lucuma.itc.client.OptimizedSpectroscopyGraphResult] =
        IO.raiseError(new java.lang.RuntimeException("optimizedSpectroscopyGraph: not implemented"))

      def spectroscopyIntegrationTimeAndGraph(
        input:    lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphInput,
        useCache: Boolean = true
      ): IO[lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphResult] =
        IO.raiseError(new java.lang.RuntimeException("spectroscopyIntegrationTimeAndGraph: not implemented"))

      override def versions: IO[ItcVersions] =
        FakeItcVersions.pure[IO]
    }

  // override in tests that need an http client
  protected def httpRequestHandler: Request[IO] => Resource[IO, Response[IO]] = _ => Resource.eval(IO.pure(Response.notFound[IO]))
  
  private def httpClient: Client[IO] = Client.apply(httpRequestHandler)

  protected def databaseConfig: Config.Database =
    Config.Database(
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

  // These are overriden in OdbSuiteWithS3 for tests that need it.
  protected def s3ClientOpsResource: Resource[IO, S3AsyncClientOp[IO]] =
    S3FileService.s3AsyncClientOpsResource[IO](awsConfig)

  protected def s3PresignerResource: Resource[IO, S3Presigner] =
    S3FileService.s3PresignerResource[IO](awsConfig)

  private def httpApp: Resource[IO, WebSocketBuilder2[IO] => HttpApp[IO]] =
    FMain.routesResource[IO](
      databaseConfig,
      awsConfig,
      itcClient.pure[Resource[IO, *]],
      CommitHash.Zero,
      ssoClient.pure[Resource[IO, *]],
      "unused",
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
      ptc <- db.evalMap(TimeEstimateCalculator.fromSession(_, enm))
      map  = OdbMapping(db, mon, usr, top, itc, CommitHash.Zero, enm, ptc, httpClient)
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
    for {
      xbe <- JdkHttpClient.simple[IO].map(Http4sHttpBackend[IO](_))
      uri  = svr.baseUri / "odb"
      xc  <- Http4sHttpClient.of[IO, Nothing](uri, headers = Headers(Authorization(Credentials.Token(AuthScheme.Bearer, Gid[User.Id].fromString.reverseGet(user.id)))))(Async[IO], xbe, Logger[IO])
    } yield xc

  private def streamingClient(user: User)(svr: Server): Resource[IO, WebSocketClient[IO, Nothing]] =
    for {
      sbe <- Resource.eval(JdkWSClient.simple[IO].map(Http4sWebSocketBackend[IO](_)))
      uri  = (svr.baseUri / "ws").copy(scheme = Some(Http4sUri.Scheme.unsafeFromString("ws")))
      sc  <- Resource.eval(Http4sWebSocketClient.of[IO, Nothing](uri)(Async[IO], Logger[IO], sbe))
      ps   = Map("Authorization" -> Json.fromString(s"Bearer ${Gid[User.Id].fromString.reverseGet(user.id)}"))
      _   <- Resource.make(sc.connect() *> sc.initialize(ps))(_ => sc.terminate() *> sc.disconnect())
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
    client:    ClientOption = ClientOption.Http,
  ): IO[Unit] = {
    val op = this.query(user, query, variables, client)
    expected.fold(errors => {
      op
      .intercept[ResponseException[Any]]
        .map { e => e.errors.toList.map(_.message) }
        .assertEquals(errors)
    }, success => {
      op.map(_.spaces2)
        .assertEquals(success.spaces2) // by comparing strings we get more useful errors
    })
  }

  def expectIor(
    user:      User,
    query:     String,
    expected:  Ior[List[String], Json],
    variables: Option[JsonObject] = None,
    client:    ClientOption = ClientOption.Http,
  ): IO[Unit] = {
    val op = this.query(user, query, variables, client)
    expected.fold(
      errors => {
        op.intercept[ResponseException[Any]]
          .map(_.errors.toList.map(_.message))
          .assertEquals(errors)
      },
      success => {
        op.map(_.spaces2)
          .assertEquals(success.spaces2) // by comparing strings we get more useful errors
      },
      (errors, success) => {
        op.intercept[ResponseException[Json]]
          .map { case ResponseException(e, d) =>
            assertEquals(e.toList.map(_.message), errors)
            assertEquals(d.map(_.spaces2), success.spaces2.some)
          }
      }
    )
  }

  def query(
    user:      User,
    query:     String,
    variables: Option[JsonObject] = None,
    client:    ClientOption = ClientOption.Http,
  ): IO[Json] =
    Resource.eval(IO(serverFixture()))
      .flatMap(client.connection(user))
      .use { conn =>
        val req = conn.request(Operation(query))
        val op  = variables.fold(req.apply)(req.withInput)
        op
      }

  def subscription(user: User, query: String, mutations: Either[List[(String, Option[JsonObject])], IO[Any]], variables: Option[JsonObject] = None): IO[List[Json]] =
    Supervisor[IO].use { sup =>
      Resource.eval(IO(serverFixture()))
        .flatMap(streamingClient(user))
        .use { conn =>
          val req = conn.subscribe(Operation(query))
          variables.fold(req.apply)(req.withInput).allocated.flatMap { case (sub, cleanup) =>
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

  def withSession[A](f: Session[IO] => IO[A]): IO[A] =
    Resource.eval(IO(sessionFixture())).use(f)

  import lucuma.odb.service.Services
  def withServices[A](u: User)(f: Services[IO] => IO[A]): IO[A] =
    Resource.eval(IO(sessionFixture())).use { s =>
      f(Services.forUser(u)(s))
    }

}
