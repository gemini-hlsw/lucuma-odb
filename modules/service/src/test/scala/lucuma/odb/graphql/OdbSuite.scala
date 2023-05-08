// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.data.NonEmptyList
import cats.effect.*
import cats.effect.std.Supervisor
import cats.implicits.*
import clue.ErrorPolicy
import clue.FetchClient
import clue.GraphQLOperation
import clue.PersistentStreamingClient
import clue.ResponseException
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import clue.http4s.Http4sWebSocketBackend
import clue.http4s.Http4sWebSocketClient
import clue.websocket.WebSocketClient
import com.dimafeng.testcontainers.PostgreSQLContainer
import com.dimafeng.testcontainers.munit.TestContainerForAll
import edu.gemini.grackle.Mapping
import edu.gemini.grackle.skunk.SkunkMonitor
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.literal.*
import io.laserdisc.pure.s3.tagless.S3AsyncClientOp
import lucuma.core.math.SignalToNoise
import lucuma.core.model.NonNegDuration
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
import lucuma.odb.graphql.OdbMapping
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.sequence.util.CommitHash
import lucuma.refined.*
import munit.CatsEffectSuite
import munit.internal.console.AnsiColors
import natchez.Trace.Implicits.noop
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.client.websocket.WSClient
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

import java.time.Duration
import scala.concurrent.duration.*

/**
 * Mixin that allows execution of GraphQL operations on a per-suite instance of the Odb, shared
 * among all tests.
 */
abstract class OdbSuite(debug: Boolean = false) extends CatsEffectSuite with TestContainerForAll with DatabaseOperations with TestSsoClient {
  private implicit val DefaultErrorPolicy: ErrorPolicy.RaiseAlways.type = ErrorPolicy.RaiseAlways

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

  override val containerDef = new PostgreSQLContainer.Def(DockerImageName.parse("postgres:14")) {
    override def createContainer(): PostgreSQLContainer = {
      val c = super.createContainer()
      c.container.withClasspathResourceMapping("/db/migration", "/docker-entrypoint-initdb.d/", BindMode.READ_ONLY)
      if (debug)
        c.container.withLogConsumer(f => jlogger.debug(s"${AnsiColors.CYAN}${f.getUtf8String().trim()}${AnsiColors.Reset}"))
      container = c
      c
    }
  }

  private implicit val log: Logger[IO] =
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
          NonEmptyList.one(FakeItcResult)
        ).pure[IO]

      override def spectroscopy(input: SpectroscopyIntegrationTimeInput, useCache: Boolean): IO[IntegrationTimeResult] =
        IntegrationTimeResult(
          FakeItcVersions,
          NonEmptyList.one(FakeItcResult)
        ).pure[IO]

      def optimizedSpectroscopyGraph(input: lucuma.itc.client.OptimizedSpectroscopyGraphInput, useCache: Boolean): IO[lucuma.itc.client.OptimizedSpectroscopyGraphResult] =
        IO.raiseError(new java.lang.RuntimeException("optimizedSpectroscopyGraph: not implemeneed"))

      override def versions: IO[ItcVersions] =
        FakeItcVersions.pure[IO]
    }

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

  // overriden in OdbSuiteWithS3 for tests that need it.
  protected def s3ClientOpsResource: Resource[IO, S3AsyncClientOp[IO]] =
    FMain.s3ClientOpsResource[IO](awsConfig)

  private def httpApp: Resource[IO, WebSocketBuilder2[IO] => HttpApp[IO]] =
    FMain.routesResource[IO](
      databaseConfig,
      awsConfig,
      itcClient.pure[Resource[IO, *]],
      CommitHash.Zero,
      ssoClient.pure[Resource[IO, *]],
      "unused",
      s3ClientOpsResource,
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
      map  = OdbMapping(db, mon, usr, top, itc, CommitHash.Zero, enm)
    } yield map

  protected def server: Resource[IO, Server] =
    for {
      a <- httpApp
      s <- BlazeServerBuilder[IO]
             .withHttpWebSocketApp(a)
             .bindAny()
             .resource
    } yield s

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

  override def munitFixtures = List(serverFixture)

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

}
