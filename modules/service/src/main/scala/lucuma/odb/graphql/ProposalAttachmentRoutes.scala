// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.data.ValidatedNel
import cats.effect._
import cats.implicits._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.Config
import lucuma.odb.data.Tag
import lucuma.odb.service.AttachmentFileService.AttachmentException
import lucuma.odb.service.ProposalAttachmentFileService
import lucuma.sso.client.SsoClient
import natchez.Trace
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.middleware.EntityLimiter

object ProposalAttachmentRoutes {
  object ProgramId {
    def unapply(str: String): Option[Program.Id] = Program.Id.parse(str)
  }

  object TagPath {
    def unapply(str: String): Option[Tag] =
      NonEmptyString.from(str).toOption.map(s => Tag(s.value.toLowerCase))
  }

  def apply[F[_]: Async: Trace](
    attachmentFileService: ProposalAttachmentFileService[F],
    ssoClient:             SsoClient[F, User],
    maxUploadMb:           Int
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
    object DescriptionMatcher extends OptionalQueryParamDecoderMatcher[String]("description")

    val routes = HttpRoutes.of[F] {
      case req @ GET -> Root / "attachment" / "proposal" / ProgramId(programId) / TagPath(attachmentType) =>
        ssoClient.require(req) { user =>
          attachmentFileService.getAttachment(user, programId, attachmentType).flatMap {
            case Left(exc)     => exc.toResponse
            case Right(stream) => Async[F].pure(Response(Status.Ok, body = stream))
          }
        }

      case req @ POST -> Root / "attachment" / "proposal" / ProgramId(programId) / TagPath(attachmentType)
          :? FileNameMatcher(fileName) +& DescriptionMatcher(optDesc) =>
        ssoClient.require(req) { user =>
          val description = optDesc.flatMap(d => NonEmptyString.from(d).toOption)
          attachmentFileService
            .insertAttachment(user, programId, attachmentType, fileName, description, req.body)
            .flatMap(_ => Ok())
            .recoverWith {
              case EntityLimiter.EntityTooLarge(_) =>
                BadRequest(s"File too large. Limit of $maxUploadMb MB")
              case e: AttachmentException          => e.toResponse
            }
        }

      case req @ PUT -> Root / "attachment" / "proposal" / ProgramId(programId) / TagPath(attachmentType)
          :? FileNameMatcher(fileName) +& DescriptionMatcher(optDesc) =>
        ssoClient.require(req) { user =>
          val description = optDesc.flatMap(d => NonEmptyString.from(d).toOption)
          attachmentFileService
            .updateAttachment(user, programId, attachmentType, fileName, description, req.body)
            .flatMap(_ => Ok())
            .recoverWith {
              case EntityLimiter.EntityTooLarge(_) =>
                BadRequest(s"File too large. Limit of $maxUploadMb MB")
              case e: AttachmentException          => e.toResponse
            }
        }

      case req @ DELETE -> Root / "attachment" / "proposal" / ProgramId(programId) / TagPath(attachmentType) =>
        ssoClient.require(req) { user =>
          attachmentFileService
            .deleteAttachment(user, programId, attachmentType)
            .flatMap(_ => Ok())
            .recoverWith { case e: AttachmentException => e.toResponse }
        }
      
      case req @ GET -> Root / "attachment" / "proposal" / "url" / ProgramId(programId) / TagPath(attachmentType) =>
        ssoClient.require(req) { user =>
          attachmentFileService
            .getPresignedUrl(user, programId, attachmentType)
            .flatMap(Ok(_))
            .recoverWith { case e: AttachmentException => e.toResponse }
        }
    }

    EntityLimiter(routes, maxUploadMb * 1000 * 1000)
  }
}
