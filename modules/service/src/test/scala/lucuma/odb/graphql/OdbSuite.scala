// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.data.OptionT
import cats.effect._
import cats.effect.std.Supervisor
import cats.implicits._
import clue.ApolloWebSocketClient
import clue.GraphQLOperation
import clue.PersistentStreamingClient
import clue.ResponseException
import clue.TransactionalClient
import clue.http4s.Http4sBackend
import clue.http4s.Http4sWSBackend
import com.dimafeng.testcontainers.PostgreSQLContainer
import com.dimafeng.testcontainers.munit.TestContainerForAll
import io.circe.Json
import io.circe.literal._
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.odb.Config
import lucuma.odb.Main
import lucuma.sso.client.SsoClient
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
import org.typelevel.ci.CIString
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

/**
 * Mixin that allows execution of GraphQL operations on a per-suite instance of the Odb, shared
 * among all tests.
 */
abstract class OdbSuite(debug: Boolean = false) extends CatsEffectSuite with TestContainerForAll {

  /** Ensure that exactly the specified errors are reported, in order. */
  def interceptGraphQL(messages: String*)(fa: IO[Any]): IO[Unit] =
    fa.attempt.flatMap {
      case Left(e: ResponseException) =>
        assertEquals(messages.toList, e.asGraphQLErrors.toList.flatten.map(_.message)).pure[IO]
      case Left(other) => IO.raiseError(other)
      case Right(a) => fail(s"Expected failure, got $a")
    }

  private val it = Iterator.from(1)

  /** Generate a new id, impurely. */
  def nextId = it.next().toLong

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

  def validUsers: List[User]

  val Bearer = CIString("Bearer")

  def authorization(jwt: String): Authorization =
    Authorization(Credentials.Token(Bearer, jwt))

  private def ssoClient: SsoClient[IO, User] =
    new SsoClient.AbstractSsoClient[IO, User] {
      def find(req: Request[IO]): IO[Option[User]] = OptionT.fromOption[IO](req.headers.get[Authorization]).flatMapF(get).value
      def get(authorization: Authorization): IO[Option[User]] =
        authorization match {
          case Authorization(Credentials.Token(Bearer, s)) =>
            Gid[User.Id].fromString.getOption(s).flatMap(id => validUsers.find(_.id === id)).pure[IO]
          case _ => none.pure[IO]
        }
    }

  private def httpApp: Resource[IO, WebSocketBuilder2[IO] => HttpApp[IO]] =
    Main.routesResource(
      Config.Database(
        host     = container.containerIpAddress,
        port     = container.mappedPort(POSTGRESQL_PORT),
        user     = container.username,
        password = container.password,
        database = container.databaseName,
      ),
      ssoClient.pure[Resource[IO, *]],
      "unused"
    ).map(_.map(_.orNotFound))

  private def server: Resource[IO, Server] =
    // Resource.make(IO.println("  • Server starting..."))(_ => IO.println("  • Server stopped.")) *>
    httpApp.flatMap { app =>
      BlazeServerBuilder[IO]
        .withHttpWebSocketApp(app)
        .bindAny()
        .resource
        // .flatTap(_ => Resource.eval(IO.println("  • Server started.")))
    }

  private def transactionalClient(user: User)(svr: Server): IO[TransactionalClient[IO, Nothing]] =
    for {
      xbe <- JdkHttpClient.simple[IO].map(Http4sBackend[IO](_))
      uri  = svr.baseUri / "odb"
      xc  <- TransactionalClient.of[IO, Nothing](uri, headers = Headers(Authorization(Credentials.Token(AuthScheme.Bearer, Gid[User.Id].fromString.reverseGet(user.id)))))(Async[IO], xbe, Logger[IO])
    } yield xc

  private def streamingClient(user: User)(svr: Server): Resource[IO, ApolloWebSocketClient[IO, Nothing]] =
    for {
      sbe <- Resource.eval(JdkWSClient.simple[IO].map(Http4sWSBackend[IO](_)))
      uri  = (svr.baseUri / "ws").copy(scheme = Some(Http4sUri.Scheme.unsafeFromString("ws")))
      sc  <- Resource.eval(ApolloWebSocketClient.of(uri)(Async[IO], Logger[IO], sbe))
      ps   = Map("Authorization" -> Json.fromString(s"Bearer ${Gid[User.Id].fromString.reverseGet(user.id)}"))
      _   <- Resource.make(sc.connect() *> sc.initialize(ps))(_ => sc.terminate() *> sc.disconnect())
    } yield sc.asInstanceOf[ApolloWebSocketClient[IO, Nothing]] // why does Nothing change to Any here?

  case class Operation(
    document: String
  ) extends GraphQLOperation[Nothing] {
    type Data       = Json
    type Variables  = Json
    val varEncoder  = implicitly
    val dataDecoder = implicitly
  }

  private lazy val serverFixture: Fixture[Server] =
    ResourceSuiteLocalFixture("server", server)

  override def munitFixtures = List(serverFixture)

  sealed trait ClientOption extends Product with Serializable {

    def prefix: String =
      this match {
        case ClientOption.Http => "http     "
        case ClientOption.Ws   => "websocket"
      }

    def connection(user: User): Server => Resource[IO, TransactionalClient[IO, Nothing]] =
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

  def expect(
    user:      User,
    query:     String,
    expected:  Either[List[String], Json],
    variables: Option[Json] = None,
    client:    ClientOption = ClientOption.Http,
  ): IO[Unit] = {
    val op = this.query(user, query, variables, client)
    expected.fold(errors => {
      op.intercept[ResponseException]
        .map { e => e.asGraphQLErrors.toList.flatten.map(_.message) }
        .assertEquals(errors)
    }, success => {
      op.map(_.spaces2)
        .assertEquals(success.spaces2) // by comparing strings we get more useful errors
    })
  }

  def query(
    user:      User,
    query:     String,
    variables: Option[Json] = None,
    client:    ClientOption = ClientOption.Http,
  ): IO[Json] =
    Resource.eval(IO(serverFixture()))
      .flatMap(client.connection(user))
      .use { conn =>
        val req = conn.request(Operation(query))
        val op  = variables.fold(req.apply)(req.apply)
        op
      }

  def subscription(user: User, query: String, mutations: Either[List[(String, Option[Json])], IO[Any]], variables: Option[Json] = None): IO[List[Json]] =
    Supervisor[IO].use { sup =>
      Resource.eval(IO(serverFixture()))
        .flatMap(streamingClient(user))
        .use { conn =>
          val req = conn.subscribe(Operation(query))
          variables.fold(req.apply)(req.apply).allocated.flatMap { case (sub, cleanup) =>
            for {
              _   <- log.info("*** ----- about to start stream fiber")
              fib <- sup.supervise(sub.compile.toList)
              _   <- log.info("*** ----- pausing a bit")
              _   <- IO.sleep(1.second)
              _   <- log.info("*** ----- running mutations")
              _   <- mutations.fold(_.traverse_ { case (query, vars) =>
                val req = conn.request(Operation(query))
                vars.fold(req.apply)(req.apply)
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

  def subscriptionExpect(user: User, query: String, mutations: Either[List[(String, Option[Json])], IO[Any]], expected: List[Json], variables: Option[Json] = None) =
    subscription(user, query, mutations, variables).map { obt =>
      assertEquals(obt.map(_.spaces2), expected.map(_.spaces2))  // by comparing strings we get more useful errors
    }

}