// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package email

import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.UUIDGen
import cats.effect.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.EmailStatus
import lucuma.odb.Config
import lucuma.odb.data.EmailId
import lucuma.odb.service.EmailWebhookService
import lucuma.refined.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.implicits.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class emailWebhookRoutes extends OdbSuite {

  val pi = TestUsers.Standard.pi(1, 101)
  val validUsers = List(pi)

  // match the signing key to the test message below
  override val emailConfig = 
    Config.Email(
      apiKey            = "apiKey".refined,
      domain            = "gpp.com".refined,
      webhookSigningKey = "55484a8372da2cf84445b8a65d674511".refined,
      invitationFrom    = EmailAddress.unsafeFrom("explore@gpp.com")
    )

  private given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  private var routes: Resource[IO, HttpApp[IO]] = null

  override def beforeAll(): Unit = {
    super.beforeAll()

    routes = 
      session
        .map(EmailWebhookService.fromSession)
        .map(EmailWebhookRoutes(_, emailConfig).orNotFound)
  }

  private val emailId: IO[EmailId] = UUIDGen[IO].randomUUID.map(uuid => EmailId.unsafeFromString(uuid.toString))
  private def request(body: Json): Request[IO] = 
    Request[IO](
      method = Method.POST,
      uri = uri"mailgun"
    ).withEntity(body)

  extension (response: Response[IO])
    def isBodyEmpty: IO[Boolean] = response.as[String].map(_.isEmpty)

  test("valid ACCEPTED") {
    for {
      pid  <- createProgramAs(pi)
      eid  <- emailId
      _    <- insertEmail(eid, pid)
      resp <- routes.use(_.run(request(validAccepted(eid))))
      stat <- getEmailStatus(eid)
    } yield {
      assertEquals(resp.status, Status.Ok)
      assertIOBoolean(resp.isBodyEmpty)
      assertEquals(stat, EmailStatus.Accepted)
    }
  }

  test("valid DELIVERED") {
    for {
      pid  <- createProgramAs(pi)
      eid  <- emailId
      _    <- insertEmail(eid, pid)
      resp <- routes.use(_.run(request(validDelivered(eid))))
      stat <- getEmailStatus(eid)
    } yield {
      assertEquals(resp.status, Status.Ok)
      assertIOBoolean(resp.isBodyEmpty)
      assertEquals(stat, EmailStatus.Delivered)
    }
  }

  test("valid PERMANENT_FAILURE") {
    for {
      pid  <- createProgramAs(pi)
      eid  <- emailId
      _    <- insertEmail(eid, pid)
      resp <- routes.use(_.run(request(validPermanentFailure(eid))))
      stat <- getEmailStatus(eid)
    } yield {
      assertEquals(resp.status, Status.Ok)
      assertIOBoolean(resp.isBodyEmpty)
      assertEquals(stat, EmailStatus.PermanentFailure)
    }
  }

  test("valid TEMPORARY_FAILURE") {
    for {
      pid  <- createProgramAs(pi)
      eid  <- emailId
      _    <- insertEmail(eid, pid)
      resp <- routes.use(_.run(request(validTemporaryFailure(eid))))
      stat <- getEmailStatus(eid)
    } yield {
      assertEquals(resp.status, Status.Ok)
      assertIOBoolean(resp.isBodyEmpty)
      assertEquals(stat, EmailStatus.TemporaryFailure)
    }
  }

  test("bad signature") {
    for {
      pid  <- createProgramAs(pi)
      eid  <- emailId
      _    <- insertEmail(eid, pid)
      resp <- routes.use(_.run(request(badSignature(eid))))
      stat <- getEmailStatus(eid)
    } yield {
      assertEquals(resp.status, Status.BadRequest)
      assertIO(resp.as[String], "\"Invalid signature\"")
      // unchanged
      assertEquals(stat, EmailStatus.Queued)
    }
  }

  private def validAccepted(emailId: EmailId) = 
    json"""
      {
        "signature":{
          "token":"ba80a5aa86eead8b685f9d3644be722756394cea4fb9ca7f78",
          "timestamp":"1712845621",
          "signature":"aba5651de3f086f5674a1d0dd864542c6c9f26bb4d5274453e4ea4a76352e378"
        },
        "event-data":{
          "event":"accepted",
          "id":"ucl8I1UERCidJ7Ec8_40PQ",
          "timestamp":1712845621.823009,
          "message":{
            "headers":{
              "message-id": ${emailId.value.value}
            }
          }
        }
      }
    """

  private def validDelivered(emailId: EmailId) = 
    json"""
      {
        "signature":{
          "token":"ba80a5aa86eead8b685f9d3644be722756394cea4fb9ca7f78",
          "timestamp":"1712845621",
          "signature":"aba5651de3f086f5674a1d0dd864542c6c9f26bb4d5274453e4ea4a76352e378"
        },
        "event-data":{
          "event":"delivered",
          "id":"ucl8I1UERCidJ7Ec8_40PQ",
          "timestamp":1712845621.823009,
          "message":{
            "headers":{
              "message-id": ${emailId.value.value}
            }
          }
        }
      }
    """

  private def validPermanentFailure(emailId: EmailId) = 
    json"""
      {
        "signature":{
          "token":"ba80a5aa86eead8b685f9d3644be722756394cea4fb9ca7f78",
          "timestamp":"1712845621",
          "signature":"aba5651de3f086f5674a1d0dd864542c6c9f26bb4d5274453e4ea4a76352e378"
        },
        "event-data":{
          "event":"failed",
          "id":"ucl8I1UERCidJ7Ec8_40PQ",
          "timestamp":1712845621.823009,
          "severity": "permanent",
          "message":{
            "headers":{
              "message-id": ${emailId.value.value}
            }
          }
        }
      }
    """

  private def validTemporaryFailure(emailId: EmailId) = 
    json"""
      {
        "signature":{
          "token":"ba80a5aa86eead8b685f9d3644be722756394cea4fb9ca7f78",
          "timestamp":"1712845621",
          "signature":"aba5651de3f086f5674a1d0dd864542c6c9f26bb4d5274453e4ea4a76352e378"
        },
        "event-data":{
          "event":"failed",
          "id":"ucl8I1UERCidJ7Ec8_40PQ",
          "timestamp":1712845621.823009,
          "severity": "temporary",
          "message":{
            "headers":{
              "message-id": ${emailId.value.value}
            }
          }
        }
      }
    """

  private def badSignature(emailId: EmailId) = 
    json"""
      {
        "signature":{
          "token":"ba80a5aa86eead8b685f9d3644be722756394cea4fb9ca7f78",
          "timestamp":"1712845621",
          "signature":"aba5651de3f086f5674a1d0dd864542c6c9f26bb4d5274453e4ea4a76352e37a"
        },
        "event-data":{
          "event":"accepted",
          "id":"ucl8I1UERCidJ7Ec8_40PQ",
          "timestamp":1712845621.823009,
          "message":{
            "headers":{
              "message-id": ${emailId.value.value}
            }
          }
        }
      }
    """

}
