// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.data.ValidatedNel
import cats.effect._
import cats.implicits._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.Config
import lucuma.odb.data.Tag
import lucuma.odb.service.AttachmentFileService.AttachmentException
import lucuma.odb.service.ObsAttachmentFileService
import lucuma.sso.client.SsoClient
import natchez.Trace
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.middleware.EntityLimiter
import skunk.Session
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import cats.effect.std.UUIDGen
import lucuma.odb.service.S3FileService

object ObsAttachmentRoutes {
  object ProgramId {
    def unapply(str: String): Option[Program.Id] = Program.Id.parse(str)
  }

  object ObsAttachmentId {
    def unapply(str: String): Option[ObsAttachment.Id] = ObsAttachment.Id.parse(str)
  }

  def apply[F[_]: Concurrent: Trace: UUIDGen](
    pool:                  Resource[F, Session[F]],
    s3:                    S3FileService[F],
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
          pool.map(Services.forUser(user)).useTransactionally {
            obsAttachmentFileService(s3).getAttachment(user, programId, attachmentId).flatMap {
              case Left(exc)     => exc.toResponse
              case Right(stream) => Response(Status.Ok, body = stream).pure[F]
            }
          }
        }

      case req @ POST -> Root / "attachment" / "obs" / ProgramId(programId)
          :? FileNameMatcher(fileName) +& TagMatcher(typeTag) +& DescriptionMatcher(optDesc) =>
        ssoClient.require(req) { user =>
          pool.map(Services.forUser(user)).useTransactionally {
            val description = optDesc.flatMap(d => NonEmptyString.from(d).toOption)
            obsAttachmentFileService(s3)
              .insertAttachment(user, programId, typeTag, fileName, description, req.body)
              .flatMap(id => Ok(id.toString))
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
          pool.map(Services.forUser(user)).useTransactionally {
            val description = optDesc.flatMap(d => NonEmptyString.from(d).toOption)
            obsAttachmentFileService(s3)
              .updateAttachment(user, programId, attachmentId, fileName, description, req.body)
              .flatMap(_ => Ok())
              .recoverWith {
                case EntityLimiter.EntityTooLarge(_) =>
                  BadRequest(s"File too large. Limit of $maxUploadMb MB")
                case e: AttachmentException          => e.toResponse
              }
          }
        }

      case req @ DELETE -> Root / "attachment" / "obs" / ProgramId(programId) / ObsAttachmentId(attachmentId) =>
        ssoClient.require(req) { user =>
          pool.map(Services.forUser(user)).useTransactionally {
            obsAttachmentFileService(s3)
              .deleteAttachment(user, programId, attachmentId)
              .flatMap(_ => Ok())
              .recoverWith { case e: AttachmentException => e.toResponse }
          }
        }
      
      case req @ GET -> Root / "attachment" / "obs" / "url" / ProgramId(programId) / ObsAttachmentId(attachmentId) =>
        ssoClient.require(req) { user =>
          pool.map(Services.forUser(user)).useTransactionally {
            obsAttachmentFileService(s3)
              .getPresignedUrl(user, programId, attachmentId)
              .flatMap(Ok(_))
              .recoverWith { case e: AttachmentException => e.toResponse }
          }
        }
    }

    EntityLimiter(routes, maxUploadMb * 1000 * 1000)
  }
}
