// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.test

import cats.effect.*
import cats.syntax.all.*
import clue.FetchClient
import clue.GraphQLOperation
import clue.ResponseException
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import clue.http4s.Http4sWebSocketBackend
import clue.http4s.Http4sWebSocketClient
import io.circe.Json
import io.circe.JsonObject
import lucuma.core.model.User
import lucuma.resource.test.ServerFixtures
import munit.Location
import org.http4s.*
import org.http4s.client.UnexpectedStatus
import org.http4s.headers.Authorization
import org.http4s.jdkhttpclient.JdkHttpClient
import org.http4s.jdkhttpclient.JdkWSClient
import org.http4s.server.Server
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait ResourceGraphQLSuite extends ServerFixtures:

  /** The user whose credentials authenticate requests by default. */
  protected def defaultUser: User = TestSso.pi

  // Credentials for the `authorization` parameter of the expect/query helpers.

  /** No credentials at all. */
  protected val anonymous: IO[Option[Authorization]] = IO.pure(none)

  /** A valid JWT for the given user. */
  protected def asUser(user: User): IO[Option[Authorization]] =
    TestSso.authorizationHeader(user).map(_.some)

  /** An arbitrary (e.g. deliberately invalid) Authorization header. */
  protected def rawAuthorization(auth: Authorization): IO[Option[Authorization]] =
    IO.pure(auth.some)

  private def defaultAuthorization: IO[Option[Authorization]] = asUser(defaultUser)

  def expect(
    query:         String,
    expected:      Either[List[String], Json],
    variables:     Option[JsonObject] = None,
    client:        ClientOption = ClientOption.Default,
    authorization: IO[Option[Authorization]] = defaultAuthorization
  )(using Location): IO[Unit] = {
    val op = this.query(query, variables, client, authorization)
    expected.fold(
      errors =>
        op
          .intercept[ResponseException[Any]]
          .map(e => e.errors.toList.map(_.message))
          .assertEquals(errors),
      success =>
        op.map(_.spaces2)
          .assertEquals(success.spaces2) // by comparing strings we get more useful errors
    )
  }

  def expectSuccess(
    query:         String,
    expected:      Json,
    variables:     Option[JsonObject] = None,
    client:        ClientOption = ClientOption.Default,
    authorization: IO[Option[Authorization]] = defaultAuthorization
  )(using Location): IO[Unit] =
    this
      .query(query, variables, client, authorization)
      .map(_.spaces2)
      .map(assertNoDiff(_, expected.spaces2)) // by comparing strings we get more useful errors

  def expectFailure(
    query:         String,
    variables:     Option[JsonObject] = None,
    client:        ClientOption = ClientOption.Default,
    authorization: IO[Option[Authorization]] = anonymous
  )(using Location): IO[Unit] =
    this
      .query(query, variables, client, authorization)
      .intercept[UnexpectedStatus]
      .map(e => assertEquals(e.status, Status.Forbidden))

  def query(
    query:         String,
    variables:     Option[JsonObject] = None,
    client:        ClientOption = ClientOption.Default,
    authorization: IO[Option[Authorization]] = defaultAuthorization
  ): IO[Json] =
    Resource
      .eval(authorization)
      .flatMap: auth =>
        Resource.eval(IO(serverFixture())).flatMap(client.connection(_, auth))
      .use: conn =>
        val req = conn.request(Operation(query))
        val op  = variables.fold(req.apply)(req.withInput).raiseGraphQLErrors
        op

  private case class Operation(document: String)
      extends GraphQLOperation.Typed[Nothing, JsonObject, Json]

enum ClientOption:
  case Http
  case Ws

  def connection(svr: Server, auth: Option[Authorization]): Resource[IO, FetchClient[IO, Nothing]] =
    this match
      case ClientOption.Http => httpClient(svr, auth)
      case ClientOption.Ws   => streamingClient(svr, auth)

  private def httpClient(
    svr:  Server,
    auth: Option[Authorization]
  ): Resource[IO, FetchClient[IO, Nothing]] =
    val uri = svr.baseUri / "resource" / "graphql"
    for
      given Http4sHttpBackend[IO] <- JdkHttpClient.simple[IO].map(Http4sHttpBackend[IO](_))
      given Logger[IO]             = Slf4jLogger.getLogger[IO]
      headers                      = auth.fold(Headers.empty)(Headers(_))
      xc                          <- Resource.eval(Http4sHttpClient.of[IO, Nothing](uri, headers = headers))
    yield xc

  private def streamingClient(
    svr:  Server,
    auth: Option[Authorization]
  ): Resource[IO, FetchClient[IO, Nothing]] =
    val uri     =
      (svr.baseUri / "resource" / "graphql").copy(scheme = Some(Uri.Scheme.unsafeFromString("ws")))
    val payload = auth.fold(Map.empty[String, Json]): a =>
      Map("Authorization" -> Json.fromString(Header[Authorization].value(a)))
    for {
      given Http4sWebSocketBackend[IO] <- JdkWSClient.simple[IO].map(Http4sWebSocketBackend[IO](_))
      given Logger[IO]                  = Slf4jLogger.getLogger[IO]
      sc                               <- Resource.eval(Http4sWebSocketClient.of[IO, Nothing](uri))
      _                                <- Resource.make(sc.connect(payload.pure[IO]))(_ => sc.disconnect())
    } yield sc

object ClientOption:
  val Default: ClientOption   = Http
  val All: List[ClientOption] = List(Http, Ws)
