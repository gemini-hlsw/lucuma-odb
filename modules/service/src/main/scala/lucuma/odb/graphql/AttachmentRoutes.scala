// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.data.ValidatedNel
import cats.effect._
import cats.implicits._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Attachment
import lucuma.core.model.GuestUser
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.Config
import lucuma.odb.data.Tag
import lucuma.odb.service.AttachmentService
import lucuma.odb.service.AttachmentService.AttachmentException
import lucuma.sso.client.SsoClient
import natchez.Trace
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.middleware.EntityLimiter

object AttachmentRoutes {
  object ProgramId {
    def unapply(str: String): Option[Program.Id] = Program.Id.parse(str)
  }

  object AttachmentId {
    def unapply(str: String): Option[Attachment.Id] = Attachment.Id.parse(str)
  }

  def apply[F[_]: Async: Trace](
    attachmentService: AttachmentService[F],
    ssoClient:         SsoClient[F, User],
    maxUploadMb:       Int
  ): HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._

    extension (exc: AttachmentException)
      def toResponse: F[Response[F]] = exc match {
        case AttachmentException.Forbidden        => Forbidden()
        case AttachmentException.FileNotFound     => NotFound()
        case AttachmentException.InvalidName(msg) => BadRequest(msg)
        case AttachmentException.InvalidType(msg) => BadRequest(msg)
      }

    given QueryParamDecoder[Tag] = QueryParamDecoder[String].map(s => Tag(s.toLowerCase))

    object FileNameMatcher    extends QueryParamDecoderMatcher[String]("fileName")
    object TagMatcher         extends QueryParamDecoderMatcher[Tag]("attachmentType")
    object DescriptionMatcher extends OptionalQueryParamDecoderMatcher[String]("description")

    val pathBase = "attachment"

    val routes = HttpRoutes.of[F] {
      case req @ GET -> Root / pathBase / ProgramId(programId) / AttachmentId(attachmentId) =>
        ssoClient.require(req) { user =>
          attachmentService.getAttachment(user, programId, attachmentId).flatMap {
            case Left(exc)     => exc.toResponse
            case Right(stream) => Async[F].delay(Response(Status.Ok, body = stream))
          }
        }

      case req @ POST -> Root / pathBase / ProgramId(programId)
          :? FileNameMatcher(fileName) +& TagMatcher(typeTag) +& DescriptionMatcher(optDesc) =>
        ssoClient.require(req) { user =>
          val description = optDesc.flatMap(d => NonEmptyString.from(d).toOption)
          attachmentService
            .uploadAttachment(user, programId, typeTag, fileName, description, req.body)
            .flatMap(id => Ok(id.toString))
            .recoverWith {
              case EntityLimiter.EntityTooLarge(_) =>
                BadRequest(s"File too large. Limit of $maxUploadMb MB")
              case e: AttachmentException          => e.toResponse
            }
        }

      case req @ DELETE -> Root / pathBase / ProgramId(programId) / AttachmentId(attachmentId) =>
        ssoClient.require(req) { user =>
          attachmentService
            .deleteAttachment(user, programId, attachmentId)
            .flatMap(_ => Ok())
            .recoverWith { case e: AttachmentException => e.toResponse }
        }
    }

    EntityLimiter(routes, maxUploadMb * 1000 * 1000)
  }
}
