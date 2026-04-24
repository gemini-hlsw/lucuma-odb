// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.test

import cats.effect.*
import clue.FetchClient
import clue.GraphQLOperation
import clue.ResponseException
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import clue.http4s.Http4sWebSocketBackend
import clue.http4s.Http4sWebSocketClient
import io.circe.Json
import io.circe.JsonObject
import lucuma.resource.test.ServerFixtures
import munit.Location
import org.http4s.*
import org.http4s.jdkhttpclient.JdkHttpClient
import org.http4s.jdkhttpclient.JdkWSClient
import org.http4s.server.Server
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait ResourceGraphQLSuite extends ServerFixtures:

  def expect(
    query:     String,
    expected:  Either[List[String], Json],
    variables: Option[JsonObject] = None,
    client:    ClientOption = ClientOption.Default
  )(using Location): IO[Unit] = {
    val op = this.query(query, variables, client)
    expected.fold(
      errors =>
        op
          .intercept[ResponseException[Any]]
          .map(e => e.errors.toList.map(_.message))
          .assertEquals(errors),
      success =>
        op.map(_.spaces2)
          //        .flatTap(s => IO.println(s))
          .assertEquals(success.spaces2) // by comparing strings we get more useful errors
    )
  }

  /** Expect success. */
  def expectSuccess(
    query:     String,
    expected:  Json,
    variables: Option[JsonObject] = None,
    client:    ClientOption = ClientOption.Default
  )(using Location): IO[Unit] =
    this
      .query(query, variables, client)
      .map(_.spaces2)
      .assertEquals(expected.spaces2) // by comparing strings we get more useful errors

  def query(
    query:     String,
    variables: Option[JsonObject] = None,
    client:    ClientOption = ClientOption.Default
  ): IO[Json] =
    Resource
      .eval(IO(serverFixture()))
      .flatMap(client.connection)
      .use:
        conn =>
          val req = conn.request(Operation(query))
          val op  = variables.fold(req.apply)(req.withInput).raiseGraphQLErrors
          op
          // op.onError:
          //   case ResponseException(es, _) =>
          //     es.traverse_ : e =>

          //       OdbError.fromGraphQLError(e) match
          //         case Some(_) => IO.unit
          //         case None    => IO.println(s"🐙 Not an OdbError: $e")
          //   case _                        => IO.unit

  private case class Operation(document: String)
      extends GraphQLOperation.Typed[Nothing, JsonObject, Json]

enum ClientOption:
  case Http
  case Ws

  def connection(svr: Server): Resource[IO, FetchClient[IO, Nothing]] = this match
    case ClientOption.Http => httpClient(svr)
    case ClientOption.Ws   => streamingClient(svr)

  private def httpClient(svr: Server): Resource[IO, FetchClient[IO, Nothing]] =
    val uri = svr.baseUri / "resource" / "graphql"
    for
      given Http4sHttpBackend[IO] <- JdkHttpClient.simple[IO].map(Http4sHttpBackend[IO](_))
      given Logger[IO]             = Slf4jLogger.getLogger[IO]
      xc                          <- Resource.eval(Http4sHttpClient.of[IO, Nothing](uri))
    yield xc

  private def streamingClient(svr: Server): Resource[IO, FetchClient[IO, Nothing]] =
    val uri =
      (svr.baseUri / "resource" / "graphql").copy(scheme = Some(Uri.Scheme.unsafeFromString("ws")))
    for {
      given Http4sWebSocketBackend[IO] <- JdkWSClient.simple[IO].map(Http4sWebSocketBackend[IO](_))
      given Logger[IO]                  = Slf4jLogger.getLogger[IO]
      sc                               <- Resource.eval(Http4sWebSocketClient.of[IO, Nothing](uri))
      // _  <- Resource.make(sc.connect(ps.pure[IO]))(_ => sc.disconnect())
    } yield sc

object ClientOption:
  val Default: ClientOption   = Http
  val All: List[ClientOption] = List(Http, Ws)
