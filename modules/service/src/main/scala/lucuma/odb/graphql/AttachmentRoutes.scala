// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.Parallel
import cats.effect.*
import cats.effect.std.SecureRandom
import cats.effect.std.UUIDGen
import cats.implicits.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.enums.AttachmentType
import lucuma.core.model.Attachment
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.horizons.HorizonsClient
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.AttachmentFileService
import lucuma.odb.service.AttachmentFileService.AttachmentException
import lucuma.odb.service.S3FileService
import lucuma.odb.service.Services
import lucuma.sso.client.SsoClient
import natchez.Trace
import org.http4s.*
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.server.middleware.EntityLimiter
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import skunk.Session

object AttachmentRoutes {
  object AttachmentId {
    def unapply(str: String): Option[Attachment.Id] = Attachment.Id.parse(str)
  }

  // the normal constructor
  def apply[F[_]: Async: Logger: LoggerFactory: Parallel: Trace: SecureRandom](
    pool:           Resource[F, Session[F]],
    s3:             S3FileService[F],
    ssoClient:      SsoClient[F, User],
    enums:          Enums,
    maxUploadMb:    Int,
    emailConfig:    Config.Email,
    commitHash:     CommitHash,
    calculator:     TimeEstimateCalculatorImplementation.ForInstrumentMode,
    httpClient:     Client[F],
    itcClient:      ItcClient[F],
    gaiaClient:     GaiaClient[F],
    horizonsClient: HorizonsClient[F]
  ): HttpRoutes[F] =
    apply(
      [A] => (u: User) => (fa: AttachmentFileService[F] => F[A]) =>
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
            TelluricTargetsClient.noop[F]
          )).map(_.attachmentFileService).use(fa),
      ssoClient,
      maxUploadMb
    )

  // used by tests
  def apply[F[_]: Async](
    service:     AttachmentFileService[F],
    ssoClient:   SsoClient[F, User],
    maxUploadMb: Int,
  ): HttpRoutes[F] =
    apply(
      [A] => (_: User) => (fa: AttachmentFileService[F] => F[A]) => fa(service),
      ssoClient,
      maxUploadMb
    )


  def apply[F[_]: Concurrent](
    service:               [A] => User => (AttachmentFileService[F] => F[A]) => F[A],
    ssoClient:             SsoClient[F, User],
    maxUploadMb:           Int
  ): HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._

    extension (exc: AttachmentException)
      def toErrorResponse: F[Response[F]] = exc match {
        case AttachmentException.Forbidden        => Forbidden()
        case AttachmentException.FileNotFound     => NotFound()
        case AttachmentException.InvalidRequest(msg) => BadRequest(msg)
      }

    extension[A](fe: F[Either[AttachmentException, A]])
      def toResponse(fa: A => F[Response[F]]): F[Response[F]] =
        fe.flatMap {
          case Right(a) => fa(a)
          case Left(e) => e.toErrorResponse
        }

    def attachmentTypeFailure(s: String): ParseFailure =
      val msg = s"Invalid attachment type '$s'"
      ParseFailure(msg, msg)
    def programIdFailure(s: String): ParseFailure =
      val msg = s"Invalid program id '$s'"
      ParseFailure(msg, msg)

    given QueryParamDecoder[AttachmentType] =
      QueryParamDecoder[String].emap(s => Enumerated[AttachmentType].fromTag(s.toLowerCase).fold(attachmentTypeFailure(s).asLeft)(_.asRight))
    given QueryParamDecoder[Program.Id] =
      QueryParamDecoder[String].emap(s => Program.Id.parse(s).fold(programIdFailure(s).asLeft)(_.asRight))

    object ProgramIdMatcher      extends QueryParamDecoderMatcher[Program.Id]("programId")
    object FileNameMatcher       extends QueryParamDecoderMatcher[String]("fileName")
    object AttachmentTypeMatcher extends QueryParamDecoderMatcher[AttachmentType]("attachmentType")
    object DescriptionMatcher    extends OptionalQueryParamDecoderMatcher[String]("description")

    val routes = HttpRoutes.of[F] {
      case req @ GET -> Root / "attachment" / AttachmentId(attachmentId) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            s.getAttachment(user, attachmentId)
              .toResponse(s => Response(Status.Ok, body = s).pure)
          }
        }

      case req @ POST -> Root / "attachment"
          :? ProgramIdMatcher(programId)
          +& FileNameMatcher(fileName)
          +& AttachmentTypeMatcher(attachmentType)
          +& DescriptionMatcher(optDesc) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            val description = optDesc.flatMap(d => NonEmptyString.from(d).toOption)
            s
              .insertAttachment(user, programId, attachmentType, fileName, description, req.body)
              .toResponse(id => Ok(id.toString))
              .recoverWith {
                case EntityLimiter.EntityTooLarge(_) =>
                  BadRequest(s"File too large. Limit of $maxUploadMb MB")
              }
          }
        }

      case req @ PUT -> Root / "attachment" / AttachmentId(attachmentId)
          :? FileNameMatcher(fileName) +& DescriptionMatcher(optDesc) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            val description = optDesc.flatMap(d => NonEmptyString.from(d).toOption)
            s
              .updateAttachment(user, attachmentId, fileName, description, req.body)
              .toResponse(_ => Ok())
              .recoverWith {
                case EntityLimiter.EntityTooLarge(_) =>
                  BadRequest(s"File too large. Limit of $maxUploadMb MB")
              }
          }
        }

      case req @ DELETE -> Root / "attachment" / AttachmentId(attachmentId) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            s
              .deleteAttachment(user, attachmentId)
              .toResponse(Ok(_))
          }
        }

      case req @ GET -> Root / "attachment" / "url" / AttachmentId(attachmentId) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            s
              .getPresignedUrl(user, attachmentId)
              .toResponse(Ok(_))
          }
        }
    }

    EntityLimiter(routes, maxUploadMb * 1000 * 1000)
  }
}
