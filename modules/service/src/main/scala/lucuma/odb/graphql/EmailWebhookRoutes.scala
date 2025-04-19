// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.Async
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import lucuma.core.enums.EmailStatus
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp
import lucuma.odb.Config
import lucuma.odb.data.EmailId
import lucuma.odb.service.EmailWebhookService
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.typelevel.log4cats.Logger
import scodec.bits.ByteVector

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

object EmailWebhookRoutes {

  object BadSignature extends RuntimeException

  // There are more events that we don't currently use, such as Opened and Clicked
  enum WebhookEventType(val tag: String) derives Enumerated:
    case Accepted extends WebhookEventType("accepted")
    case Delivered extends WebhookEventType("delivered")
    case Failed extends WebhookEventType("failed")

  enum WebhookSeverity(val tag: String) derives Enumerated:
    case Permanent extends WebhookSeverity("permanent")
    case Temporary extends WebhookSeverity("temporary")

  // We'll just treat the signature timestamp as a string - only used for signature validation
  case class Signature(timestamp: String, token: String, signature: String) derives Decoder {
    def isValid(key: NonEmptyString): Boolean = {
      val secret = SecretKeySpec(key.value.getBytes, "HmacSHA256")
      val mac = Mac.getInstance("HmacSHA256")
      mac.init(secret)
      val calculated = ByteVector(mac.doFinal((timestamp + token).getBytes)).toHex
      calculated === signature
    }
  }

  case class EventData(
    eventId: String,
    messageId: EmailId,
    eventType: WebhookEventType,
    severity: Option[WebhookSeverity],
    reason: Option[String],
    timestamp: Timestamp):
      import WebhookEventType.*
      val emailStatus: EmailStatus = eventType match {
        case Accepted => EmailStatus.Accepted
        case Delivered => EmailStatus.Delivered
        case Failed if severity === WebhookSeverity.Permanent.some => EmailStatus.PermanentFailure
        case Failed => EmailStatus.TemporaryFailure
      }

  case class WebhookEvent(signature: Signature, eventData: EventData)

  given Decoder[WebhookEventType] = Decoder.decodeString.emap(s => Enumerated[WebhookEventType].fromTag(s).toRight(s"Could not parse 'event' of $s"))
  given Decoder[WebhookSeverity] = Decoder.decodeString.emap(s => Enumerated[WebhookSeverity].fromTag(s).toRight(s"Could not parse 'severity' of $s"))

  given Decoder[Timestamp] = Decoder.decodeDouble.emap(d => Timestamp.ofEpochMilli((d * 1000).toLong).toRight(s"Invalid Timestamp: $d"))

  given Decoder[EventData] = Decoder.instance(c =>
    for {
      evtId    <- c.downField("id").as[String]
      msgId    <- c.downField("message").downField("headers").downField("message-id").as[EmailId]
      event    <- c.downField("event").as[WebhookEventType]
      severity <- c.downField("severity").as[Option[WebhookSeverity]]
      reason   <- c.downField("reason").as[Option[String]]
      ts       <- c.downField("timestamp").as[Timestamp]
    } yield EventData(evtId, msgId, event, severity, reason, ts)
  )

  given Decoder[WebhookEvent] = Decoder.instance(c =>
      for {
        sig  <- c.downField("signature").as[Signature]
        data <- c.downField("event-data").as[EventData]
      } yield WebhookEvent(sig, data)
    )

  def apply[F[_]: Async: Logger](
    webhookService: EmailWebhookService[F],
    emailConfig: Config.Email
  ): HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl.*

    def updateStatus(data: EventData): F[Unit] =
      webhookService.updateStatus(data.messageId, data.emailStatus, data.timestamp)

    def validateSignature(event: WebhookEvent): F[Unit] =
      if (event.signature.isValid(emailConfig.webhookSigningKey)) Async[F].unit
      else {
        Logger[F].warn(s"Received email webhook with invalid signature: $event") 
        throw BadSignature
      }

    val routes = HttpRoutes.of[F] {
      case req @ POST -> Root / "mailgun" =>
        (for {
          event <- req.as[WebhookEvent]
          _     <- Logger[F].debug(s"Received webhook event: $event")
          _     <- validateSignature(event)
          _     <- updateStatus(event.eventData)
          resp  <- Ok()
        } yield resp)
          .recoverWith {
            case BadSignature => BadRequest("Invalid signature")
          }
    }

    routes
  }
}

