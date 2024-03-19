// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect._
import cats.effect.implicits._
import cats.effect.std.UUIDGen
import cats.implicits._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Tag
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.service.AttachmentFileService.AttachmentException
import lucuma.odb.service.ObsAttachmentFileService
import lucuma.odb.service.S3FileService
import lucuma.odb.service.Services
import lucuma.sso.client.SsoClient
import natchez.Trace
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.middleware.EntityLimiter
import skunk.Session

object ObsAttachmentRoutes {
  object ProgramId {
    def unapply(str: String): Option[Program.Id] = Program.Id.parse(str)
  }

  object ObsAttachmentId {
    def unapply(str: String): Option[ObsAttachment.Id] = ObsAttachment.Id.parse(str)
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
      [A] => (u: User) => (fa: ObsAttachmentFileService[F] => F[A]) => pool.map(Services.forUser(u, enums)).map(_.obsAttachmentFileService(s3)).use(fa),
      ssoClient,
      maxUploadMb
    )

  // used by tests
  def apply[F[_]: Async: Trace](
    service:     ObsAttachmentFileService[F],
    ssoClient:   SsoClient[F, User],
    maxUploadMb: Int,
  ): HttpRoutes[F] =
    apply(
      [A] => (u: User) => (fa: ObsAttachmentFileService[F] => F[A]) => fa(service),
      ssoClient,
      maxUploadMb
    )


  def apply[F[_]: Concurrent: Trace: UUIDGen](
    service:               [A] => User => (ObsAttachmentFileService[F] => F[A]) => F[A],
    ssoClient:             SsoClient[F, User],
    maxUploadMb:           Int
  ): HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._

    extension (exc: AttachmentException)
      def toResponse: F[Response[F]] = exc match {
        case AttachmentException.Forbidden        => Forbidden()
        case AttachmentException.FileNotFound     => NotFound()
        case AttachmentException.InvalidRequest(msg) => BadRequest(msg)
      }

    given QueryParamDecoder[Tag] = QueryParamDecoder[String].map(s => Tag(s.toLowerCase))

    object FileNameMatcher    extends QueryParamDecoderMatcher[String]("fileName")
    object TagMatcher         extends QueryParamDecoderMatcher[Tag]("attachmentType")
    object DescriptionMatcher extends OptionalQueryParamDecoderMatcher[String]("description")

    val routes = HttpRoutes.of[F] {
      case req @ GET -> Root / "attachment" / "obs" / ProgramId(programId) / ObsAttachmentId(attachmentId) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            s.getAttachment(user, programId, attachmentId).flatMap {
              case Left(exc)     => exc.toResponse
              case Right(stream) => Response(Status.Ok, body = stream).pure[F]
            }
          }
        }

      case req @ POST -> Root / "attachment" / "obs" / ProgramId(programId)
          :? FileNameMatcher(fileName) +& TagMatcher(typeTag) +& DescriptionMatcher(optDesc) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            val description = optDesc.flatMap(d => NonEmptyString.from(d).toOption)
            s
              .insertAttachment(user, programId, typeTag, fileName, description, req.body)
              .flatMap(id => Ok(id.toString))
              .guarantee(req.body.compile.drain)
              .recoverWith {
                case EntityLimiter.EntityTooLarge(_) =>
                  BadRequest(s"File too large. Limit of $maxUploadMb MB")
                case e: AttachmentException          => e.toResponse
              }
          }
        }

      case req @ PUT -> Root / "attachment" / "obs" / ProgramId(programId) / ObsAttachmentId(attachmentId)
          :? FileNameMatcher(fileName) +& DescriptionMatcher(optDesc) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            val description = optDesc.flatMap(d => NonEmptyString.from(d).toOption)
            s
              .updateAttachment(user, programId, attachmentId, fileName, description, req.body)
              .flatMap(_ => Ok())
              .guarantee(req.body.compile.drain)
              .recoverWith {
                case EntityLimiter.EntityTooLarge(_) =>
                  BadRequest(s"File too large. Limit of $maxUploadMb MB")
                case e: AttachmentException          => e.toResponse
              }
          }
        }

      case req @ DELETE -> Root / "attachment" / "obs" / ProgramId(programId) / ObsAttachmentId(attachmentId) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            s
              .deleteAttachment(user, programId, attachmentId)
              .flatMap(_ => Ok())
              .recoverWith { case e: AttachmentException => e.toResponse }
          }
        }

      case req @ GET -> Root / "attachment" / "obs" / "url" / ProgramId(programId) / ObsAttachmentId(attachmentId) =>
        ssoClient.require(req) { user =>
          service(user) { s =>
            s
              .getPresignedUrl(user, programId, attachmentId)
              .flatMap(Ok(_))
              .recoverWith { case e: AttachmentException => e.toResponse }
          }
        }
    }

    EntityLimiter(routes, maxUploadMb * 1000 * 1000)
  }
}
