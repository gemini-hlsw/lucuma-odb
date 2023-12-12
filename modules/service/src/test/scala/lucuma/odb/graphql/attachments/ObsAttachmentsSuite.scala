// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package   attachments

import cats.Order.given
import cats.effect.IO
import cats.effect.Resource
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Program
import lucuma.core.model.User
import org.http4s.*

abstract class ObsAttachmentsSuite extends AttachmentsSuite {
  
  case class TestAttachment(
    fileName:       String,
    attachmentType: String,
    description:    Option[String],
    content:        String,
    checked:        Boolean = false
  ) {
    val upperType: String = attachmentType.toUpperCase
  }

  def insertAttachment(
    user:      User,
    programId: Program.Id,
    ta:        TestAttachment
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      val uri =
        (svr.baseUri / "attachment" / "obs" / programId.toString)
          .withQueryParam("fileName", ta.fileName)
          .withQueryParam("attachmentType", ta.attachmentType)
          .withOptionQueryParam("description", ta.description)

      val request = Request[IO](
        method = Method.POST,
        uri = uri,
        headers = Headers(authHeader(user))
      ).withEntity(ta.content)

      client.run(request)
    }
  
  def updateAttachment(
    user:         User,
    programId:    Program.Id,
    attachmentId: ObsAttachment.Id,
    ta:           TestAttachment
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      val uri =
        (svr.baseUri / "attachment" / "obs" / programId.toString / attachmentId.toString)
          .withQueryParam("fileName", ta.fileName)
          .withOptionQueryParam("description", ta.description)

      val request = Request[IO](
        method = Method.PUT,
        uri = uri,
        headers = Headers(authHeader(user))
      ).withEntity(ta.content)

      client.run(request)
    }

  def getAttachment(
    user:         User,
    programId:    Program.Id,
    attachmentId: ObsAttachment.Id
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      val uri     = svr.baseUri / "attachment" / "obs" / programId.toString / attachmentId.toString
      val request = Request[IO](
        method = Method.GET,
        uri = uri,
        headers = Headers(authHeader(user))
      )

      client.run(request)
    }

  def getPresignedUrl(
    user:         User,
    programId:    Program.Id,
    attachmentId: ObsAttachment.Id
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      val uri     = svr.baseUri / "attachment" / "obs" / "url" / programId.toString / attachmentId.toString
      val request = Request[IO](
        method = Method.GET,
        uri = uri,
        headers = Headers(authHeader(user))
      )

      client.run(request)
    }

  def deleteAttachment(
    user:         User,
    programId:    Program.Id,
    attachmentId: ObsAttachment.Id
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      val uri     = svr.baseUri / "attachment" / "obs" / programId.toString / attachmentId.toString
      val request = Request[IO](
        method = Method.DELETE,
        uri = uri,
        headers = Headers(authHeader(user))
      )

      client.run(request)
    }

  def expectedAttachments(
    attachments: List[(ObsAttachment.Id, TestAttachment)]
  ): Json =
    Json.obj(
      "obsAttachments" -> Json.fromValues(
        attachments.sortBy(_._1).map((tid, ta) =>
          Json.obj(
            "id"             -> tid.asJson,
            "attachmentType" -> ta.attachmentType.toUpperCase.asJson,
            "fileName"       -> ta.fileName.asJson,
            "description"    -> ta.description.asJson,
            "checked"        -> ta.checked.asJson,
            "fileSize"       -> ta.content.length.asJson
          )
        )
      )
    )

  val ObsAttachmentsGraph: String =
    """obsAttachments {
       |  id
       |  attachmentType
       |  fileName
       |  description
       |  checked
       |  fileSize
       |}""".stripMargin
}
