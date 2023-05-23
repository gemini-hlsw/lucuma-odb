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

object ObsAttachmentRoutes {
  object ProgramId {
    def unapply(str: String): Option[Program.Id] = Program.Id.parse(str)
  }

  object ObsAttachmentId {
    def unapply(str: String): Option[ObsAttachment.Id] = ObsAttachment.Id.parse(str)
  }

  def apply[F[_]: Async: Trace](
    attachmentFileService: ObsAttachmentFileService[F],
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
          attachmentFileService.getAttachment(user, programId, attachmentId).flatMap {
            case Left(exc)     => exc.toResponse
            case Right(stream) => Async[F].pure(Response(Status.Ok, body = stream))
          }
        }

      case req @ POST -> Root / "attachment" / "obs" / ProgramId(programId)
          :? FileNameMatcher(fileName) +& TagMatcher(typeTag) +& DescriptionMatcher(optDesc) =>
        ssoClient.require(req) { user =>
          val description = optDesc.flatMap(d => NonEmptyString.from(d).toOption)
          attachmentFileService
            .insertAttachment(user, programId, typeTag, fileName, description, req.body)
            .flatMap(id => Ok(id.toString))
            .recoverWith {
              case EntityLimiter.EntityTooLarge(_) =>
                BadRequest(s"File too large. Limit of $maxUploadMb MB")
              case e: AttachmentException          => e.toResponse
            }
        }

      case req @ PUT -> Root / "attachment" / "obs" / ProgramId(programId) / ObsAttachmentId(attachmentId)
          :? FileNameMatcher(fileName) +& DescriptionMatcher(optDesc) =>
        ssoClient.require(req) { user =>
          val description = optDesc.flatMap(d => NonEmptyString.from(d).toOption)
          attachmentFileService
            .updateAttachment(user, programId, attachmentId, fileName, description, req.body)
            .flatMap(_ => Ok())
            .recoverWith {
              case EntityLimiter.EntityTooLarge(_) =>
                BadRequest(s"File too large. Limit of $maxUploadMb MB")
              case e: AttachmentException          => e.toResponse
            }
        }

      case req @ DELETE -> Root / "attachment" / "obs" / ProgramId(programId) / ObsAttachmentId(attachmentId) =>
        ssoClient.require(req) { user =>
          attachmentFileService
            .deleteAttachment(user, programId, attachmentId)
            .flatMap(_ => Ok())
            .recoverWith { case e: AttachmentException => e.toResponse }
        }
      
      case req @ GET -> Root / "attachment" / "obs" / "url" / ProgramId(programId) / ObsAttachmentId(attachmentId) =>
        ssoClient.require(req) { user =>
          attachmentFileService
            .getPresignedUrl(user, programId, attachmentId)
            .flatMap(Ok(_))
            .recoverWith { case e: AttachmentException => e.toResponse }
        }
    }

    EntityLimiter(routes, maxUploadMb * 1000 * 1000)
  }
}
