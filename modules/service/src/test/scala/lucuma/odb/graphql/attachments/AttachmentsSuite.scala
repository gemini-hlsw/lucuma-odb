// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package attachments

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.text.utf8
import lucuma.core.model.ObsAttachment
import org.http4s.*
import org.http4s.client.Client
import org.http4s.client.JavaNetClientBuilder
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

abstract class AttachmentsSuite extends OdbSuiteWithS3 {
  
  // this logger is turned off to silence some errors (see logback.xml)
  override implicit val log: Logger[IO] =
    Slf4jLogger.getLoggerFromName("lucuma-odb-test-attachments")

  val client: Client[IO] = JavaNetClientBuilder[IO].create

  case class TestAttachment(
    fileName:       String,
    attachmentType: String,
    description:    Option[String],
    content:        String,
    checked:        Boolean = false
  ) {
    val upperType: String = attachmentType.toUpperCase
  }

  extension (response: Response[IO])
    def getBody: IO[String] = response.body.through(utf8.decode).compile.string

  extension (response: Resource[IO, Response[IO]])
    def withExpectation(expectedStatus: Status, expectedBody: String = ""): IO[Unit] =
      response
        .use(resp => resp.getBody.map(s => (resp.status, s)))
        .assertEquals((expectedStatus, expectedBody))

    def expectBody(body: String): IO[Unit] = withExpectation(Status.Ok, body)

    def expectOk: IO[Unit] = withExpectation(Status.Ok)

    def toAttachmentId: IO[ObsAttachment.Id] =
      response.use { resp =>
        for {
          _   <- IO(resp.status).assertEquals(Status.Ok)
          aid <- resp.getBody.map(s => ObsAttachment.Id.parse(s).get)
        } yield aid
      }
    
    def toNonEmptyString: IO[NonEmptyString] =
      response.use { resp =>
        for {
          _   <- IO(resp.status).assertEquals(Status.Ok)
          nes <- resp.getBody.map(s => NonEmptyString.unsafeFrom(s))
        } yield nes
      }

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 30)
  val service = TestUsers.service(3)

  val validUsers = List(pi, pi2, service)

  def getViaPresignedUrl(url: NonEmptyString): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      var uri = Uri.unsafeFromString(url.value)
      var request = Request[IO](
        method = Method.GET,
        uri = uri,
      )

      client.run(request)
    }
}
