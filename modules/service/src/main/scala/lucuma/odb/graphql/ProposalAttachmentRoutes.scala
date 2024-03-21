// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.*
import cats.implicits.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Tag
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.service.AttachmentFileService.AttachmentException
import lucuma.odb.service.ProposalAttachmentFileService
import lucuma.odb.service.S3FileService
import lucuma.odb.service.Services
import lucuma.sso.client.SsoClient
import natchez.Trace
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.middleware.EntityLimiter
import skunk.Session

object ProposalAttachmentRoutes {
  object ProgramId {
    def unapply(str: String): Option[Program.Id] = Program.Id.parse(str)
  }

  object TagPath {
    def unapply(str: String): Option[Tag] =
      NonEmptyString.from(str).toOption.map(s => Tag(s.value.toLowerCase))
  }

  // the normal constructor
  def apply[F[_]: Async: Trace](
    pool:                  Resource[F, Session[F]],
    s3:                    S3FileService[F],
    ssoClient:             SsoClient[F, User],
    enums:                 Enums,
    maxUploadMb:           Int,
  ): HttpRoutes[F] =
    apply(
      [A] => (u: User) => (fa :ProposalAttachmentFileService[F] => F[A]) => pool.map(Services.forUser(u, enums)).map(_.proposalAttachmentFileService(s3)).use(fa),
      ssoClient,
      maxUploadMb
    )

  // used by tests
  def apply[F[_]: Async: Trace](
    service:     ProposalAttachmentFileService[F],
    ssoClient:   SsoClient[F, User],
    maxUploadMb: Int,
  ): HttpRoutes[F] =
    apply(
      [A] => (u: User) => (fa :ProposalAttachmentFileService[F] => F[A]) => fa(service),
      ssoClient,
      maxUploadMb
    )

  def apply[F[_]: Async: Trace](
    service:      [A] => User => (ProposalAttachmentFileService[F] => F[A]) => F[A],
    ssoClient:    SsoClient[F, User],
    maxUploadMb:  Int
  ): HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._

    extension (exc: AttachmentException)
      def toResponse: F[Response[F]] = exc match {
        case AttachmentException.Forbidden           => Forbidden()
        case AttachmentException.FileNotFound        => NotFound()
        case AttachmentException.InvalidRequest(msg) => BadRequest(msg)
      }

    given QueryParamDecoder[Tag] = QueryParamDecoder[String].map(s => Tag(s.toLowerCase))

    object FileNameMatcher    extends QueryParamDecoderMatcher[String]("fileName")
    object TagMatcher         extends QueryParamDecoderMatcher[Tag]("attachmentType")

    val routes = HttpRoutes.of[F] {
      case req @ GET -> Root / "attachment" / "proposal" / ProgramId(programId) / TagPath(attachmentType) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            s.getAttachment(user, programId, attachmentType).flatMap {
              case Left(exc)     => exc.toResponse
              case Right(stream) => Async[F].pure(Response(Status.Ok, body = stream))
            }
          }
        }

      case req @ POST -> Root / "attachment" / "proposal" / ProgramId(programId) / TagPath(attachmentType)
          :? FileNameMatcher(fileName) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            s
              .insertAttachment(user, programId, attachmentType, fileName, req.body)
              .flatMap(_ => Ok())
              .recoverWith {
                case EntityLimiter.EntityTooLarge(_) =>
                  BadRequest(s"File too large. Limit of $maxUploadMb MB")
                case e: AttachmentException          => e.toResponse
              }
            }
        }

      case req @ PUT -> Root / "attachment" / "proposal" / ProgramId(programId) / TagPath(attachmentType)
          :? FileNameMatcher(fileName) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            s
              .updateAttachment(user, programId, attachmentType, fileName,  req.body)
              .flatMap(_ => Ok())
              .recoverWith {
                case EntityLimiter.EntityTooLarge(_) =>
                  BadRequest(s"File too large. Limit of $maxUploadMb MB")
                case e: AttachmentException          => e.toResponse
              }
          }
        }

      case req @ DELETE -> Root / "attachment" / "proposal" / ProgramId(programId) / TagPath(attachmentType) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            s
              .deleteAttachment(user, programId, attachmentType)
              .flatMap(_ => Ok())
              .recoverWith { case e: AttachmentException => e.toResponse }
          }
        }

      case req @ GET -> Root / "attachment" / "proposal" / "url" / ProgramId(programId) / TagPath(attachmentType) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            s
              .getPresignedUrl(user, programId, attachmentType)
              .flatMap(Ok(_))
              .recoverWith { case e: AttachmentException => e.toResponse }
          }
        }
    }

    EntityLimiter(routes, maxUploadMb * 1000 * 1000)
  }
}
