// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.data.OptionT
import cats.effect._
import cats.implicits._
import cats.kernel.Order
import grackle.Operation
import grackle.Result
import grackle.skunk.SkunkMonitor
import io.circe.Json
import lucuma.core.model.User
import lucuma.graphql.routes.GraphQLService
import lucuma.graphql.routes.HttpRouteHandler
import lucuma.graphql.routes.{Routes => LucumaGraphQLRoutes}
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.logic.TimeEstimateCalculator
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.UserService
import lucuma.odb.util.Cache
import lucuma.sso.client.SsoClient
import natchez.Trace
import org.http4s.Header
import org.http4s.HttpRoutes
import org.http4s.MediaType
import org.http4s.Request
import org.http4s.Response
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Authorization
import org.http4s.headers.`Content-Type`
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import skunk.Session
import skunk.SqlState

import scala.concurrent.duration._


object GraphQLRoutes {

  implicit val x: Order[Authorization] =
    Order.by(Header[Authorization].value)

  /**
   * Construct a source of `HttpRoutes` tailored to the requesting user. Routes will be cached
   * based on the `Authorization` header and discarded when `ttl` expires.
   */
  def apply[F[_]: Async: Trace: Logger](
    itcClient:  ItcClient[F],
    commitHash: CommitHash,
    ssoClient:  SsoClient[F, User],
    pool:       Resource[F, Session[F]],
    monitor:    SkunkMonitor[F],
    ttl:        FiniteDuration,
    userSvc:    UserService[F],
    enums:      Enums,
    ptc:        TimeEstimateCalculator.ForInstrumentMode,
    httpClient: Client[F]
  ): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    OdbMapping.Topics(pool).flatMap { topics =>

      // Sometimes we get invalid cursors on startup; this works around the error by doing the thing again.
      extension [A](fa: F[A]) def retryOnInvalidCursorName: F[A] =
        fa.recoverWith {
          case SqlState.InvalidCursorName(_) =>
            Logger[F].warn(s"Invalid cursor; retrying (once).") >> fa
        }

      // Log a message with the user
      def info(user: User, message: String): F[Unit] =
        Logger[F].info(s"${user.id}/${user.displayName}: $message")

      def warn(user: User, message: String): F[Unit] =
        Logger[F].warn(s"${user.id}/${user.displayName}: $message")

      def debug(user: User, message: String): F[Unit] =
        Logger[F].debug(s"${user.id}/${user.displayName}: $message")

      Cache.timed[F, Authorization, Option[GraphQLService[F]]](ttl).map { cache => wsb =>
        LucumaGraphQLRoutes.forService[F](
          {
            case None    => none.pure[F]  // No auth, no service (for now)
            case Some(a) =>
              cache.get(a).flatMap {
                case Some(opt) =>
                  Logger[F].debug(s"Cache hit for $a").as(opt) // it was in the cache
                case None    =>           // It was not in the cache
                  Logger[F].debug(s"Cache miss for $a") *>
                  {
                    for {
                      user <- OptionT(ssoClient.get(a))

                      // If the user has never hit the ODB using http then there will be no user
                      // entry in the database. So go ahead and [re]canonicalize here to be sure.
                      _    <- OptionT.liftF(userSvc.canonicalizeUser(user).retryOnInvalidCursorName)

                      _    <- OptionT.liftF(info(user, s"New service instance."))
                      map   = OdbMapping(pool, monitor, user, topics, itcClient, commitHash, enums, ptc, httpClient)
                      svc   = new GraphQLService(map) {
                        override def query(request: Operation): F[Result[Json]] =
                          super.query(request).retryOnInvalidCursorName.flatTap {
                            case Result.InternalError(t)  => warn(user, s"Internal error: ${t.getClass.getSimpleName}: ${t.getMessage}")
                            case _ => debug(user, s"Query (success).")
                          }
                      }
                    } yield svc
                  } .widen[GraphQLService[F]]
                    .value
                    .flatTap(os => cache.put(a, os))
              }
          },
          wsb,
          "odb"
        )
      }
    }

  /** 
   * An endpoint that listens on `/export/<name>` and returns an application/javascript response
   * of the form `export const <name> = '<query result>'`.
   */
  def exportConst[F[_]: Temporal](
    service: GraphQLService[F],
    name:    String,
    query:   String,
  ): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}; import dsl._
    // borrow HttpRouteHandler from lucuma-graphql-routes but hack it to return js
    val h = new HttpRouteHandler(service) {
      override def toResponse(result: Result[Json]): F[Response[F]] =
        result.toEither match {
          case Left(err)   => super.toResponse(result)
          case Right(json) => 
            Ok(s"export const $name ='${json.noSpaces}'")
            .map(_.withContentType(`Content-Type`(MediaType.application.javascript)))
        }
    }
    HttpRoutes.of[F] {
      case req @ GET -> Root / "export" / `name` =>
        h.oneOffGet(query, None, None)
    }
  }

  def enumMetadata[F[_]: Temporal](
    service: GraphQLService[F]
  ): HttpRoutes[F] =
    exportConst(service, "enumMetadata", """
      query {
        partnerMeta {
          tag
          shortName
          longName
          active
        }
        filterTypeMeta {
          tag
          shortName
          longName
        }
        obsAttachmentTypeMeta {
          tag
          shortName
          longName
          fileExtensions {
            fileExtension
          }
        }
        proposalAttachmentTypeMeta {
          tag
          shortName
          longName
        }
        proposalStatusMeta {
          tag
          name
        }
      }
    """)

}


