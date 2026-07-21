// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.Parallel
import cats.effect.Async
import cats.effect.Resource
import cats.effect.std.SecureRandom
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import grackle.Result
import grackle.Result.Failure
import grackle.Result.InternalError
import grackle.Result.Success
import grackle.Result.Warning
import io.circe.Decoder
import io.circe.DecodingFailure
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.goa.GoaClient
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.model.GuestUser
import lucuma.core.model.Program
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.horizons.HorizonsClient
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.S3FileService
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.service.UserService
import lucuma.sso.client.SsoClient
import org.http4s.*
import org.http4s.circe.jsonOf
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.otel4s.trace.Tracer
import skunk.Session

/** 
 * A single endpoint to allow the SSO service to change program ownership from a given
 * guest user to a given standard user. This is used when a guest user authenticates as
 * an existing user. This is a REST endpoint to minimize coupling.
 */
object ChownRoutes:

  extension [F[_]: Async, A, B] (r: Resource[F, A]) def useGiven(f: A ?=> F[B]): F[B] =
    r.use(a => f(using a))

  extension [A, B](e: Either[DecodingFailure, A]) def collect(f: PartialFunction[A, B]): Either[DecodingFailure, B] =
    e.flatMap(a => f.lift(a).toRight(DecodingFailure("Collect failed.", Nil)))

  // Body is JSON of the form `{ "from": <user>, "to": <user>}` via the SSO user codec.
  final case class ChownBody(from: GuestUser, to: StandardUser)
  object ChownBody:
    import lucuma.sso.client.codec.user.given
    given Decoder[ChownBody] = hc =>
      for 
        g <- hc.downField("from").as[User].collect { case g: GuestUser => g }
        s <- hc.downField("to").as[User].collect { case g: StandardUser => g }
      yield ChownBody(g, s)
    
  // N.B. very few of these arguments are actually required, but they're available at
  // the call site and it prevents us from relying on null to instantiate `Services`.
  def apply[F[_]: Async: Parallel: Logger: LoggerFactory: Tracer: SecureRandom: NoTransaction](
    pool:           Resource[F, Session[F]],
    s3:             S3FileService[F],
    ssoClient:      SsoClient[F, User],
    enums:          Enums,
    emailConfig:    Config.Email,
    commitHash:     CommitHash,
    calculator:     TimeEstimateCalculatorImplementation.ForInstrumentMode,
    httpClient:     Client[F],
    itcClient:      ItcClient[F],
    gaiaClient:     GaiaClient[F],
    horizonsClient: HorizonsClient[F]
  ): HttpRoutes[F] =
    val dsl = Http4sDsl[F]; import dsl.*
    given EntityDecoder[F, ChownBody] = jsonOf
    HttpRoutes.of[F]:
      case req @ POST -> Root / "chown" =>

        // Require a user
        ssoClient.require(req):

          // Even better, require a service user
          case u @ ServiceUser(_, _) =>

            // Require an encoded `ChownBody` as the payload
            req.as[ChownBody].flatMap:              
              case ChownBody(guest, standard) =>
              
                // Everything is cromulent at the point so get our Services instance
                pool.map(
                  Services.forUser(
                    u,
                    enums,
                    None,
                    emailConfig,
                    commitHash,
                    calculator,
                    httpClient,
                    itcClient,
                    gaiaClient,
                    s3,
                    horizonsClient,
                    TelluricTargetsClient.noop[F],
                    GoaClient.noop[F]
                  )).useGiven:

                      // This request is coming from SSO so it's possible (although unlikely) that one or 
                      // both of the users is unknown to the ODB. So we canonicalize them just to be sure.
                      def canonicalizeUsers: F[Unit] =
                        Services.asSuperUser:
                          val userService = UserService.fromSession(session)
                          userService.canonicalizeUser(guest) >>
                          userService.canonicalizeUser(standard)

                      // The chown operation is encapculated in the ProgramService
                      def chown: F[Result[List[Program.Id]]] =
                        requireServiceAccess:
                          services.transactionally:
                            programUserService.chown(guest, standard)

                      // Do the thing and map the result to an HTTP response
                      (canonicalizeUsers >> chown).flatMap:
                        case Success(Nil)             => Ok(s"User ${guest.id} owns no programs, so nothing was done.")
                        case Success(value)           => Ok(s"Changed ownership from ${guest.id} to ${standard.id} for ${value.mkString(", ")}.")
                        case Warning(problems, value) => Ok(s"Changed ownership from ${guest.id} to ${standard.id} for ${value.mkString(", ")} with warnings:\n${problems.toList.mkString("\n")}.")
                        case Failure(problems)        => InternalServerError(problems.toList.mkString("\n"))
                        case InternalError(error)     => InternalServerError(error.getMessage + "\n" + error.getStackTrace().mkString("\n"))

          // Not a service user, reject with no information given.
          case _ => Forbidden()

