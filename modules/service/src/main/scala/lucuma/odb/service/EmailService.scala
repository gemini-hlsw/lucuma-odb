// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import io.circe.Decoder
import lucuma.core.data.EmailAddress
import lucuma.core.model.Program
import lucuma.core.util.Timestamp
import lucuma.odb.Config
import lucuma.odb.data.EmailId
import lucuma.odb.data.EmailStatus
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.util.Codecs.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.*
import org.http4s.headers.Authorization
import skunk.AppliedFragment
import skunk.Session
import skunk.Transaction
import skunk.implicits.*

import Services.Syntax.*

trait EmailService[F[_]] {
  def send(
    programId: Program.Id,
    from: EmailAddress,
    to: EmailAddress,
    subject: NonEmptyString,
    textMessage: NonEmptyString,
    htmlMessage: Option[NonEmptyString]
  )(using Transaction[F]): F[Result[EmailId]]
}

object EmailService {

  // This is called from the EmailWebhookRoutes, which doesn't have access to the Services
  def updateStatus[F[_]: MonadCancelThrow](
    session: Session[F],
    emailId: EmailId,
    status: EmailStatus,
    timestamp: Timestamp
  ): F[Unit] = {
    val af = Statements.updateStatus(emailId, status, timestamp)
    
    session.prepareR(af.fragment.command)
      .use(_.execute(af.argument).void)
  }

  def fromConfigAndClient[F[_]: Concurrent](
    config:  Config.Email,
    client:  Client[F]
  )(using Services[F]): EmailService[F] = {

    new EmailService[F] {
      val authHeader = Authorization(BasicCredentials("api", config.apiKey.value))

      case class SendResponse(id: EmailId, message: String) derives Decoder

      override def send(
        programId: Program.Id,
        from: EmailAddress,
        to: EmailAddress,
        subject: NonEmptyString,
        textMessage: NonEmptyString,
        htmlMessage: Option[NonEmptyString]
      )(using Transaction[F]): F[Result[EmailId]] = {

        def insertEmail(emailId: EmailId): F[Result[Unit]] = {
          val af = Statements.insertEmail(emailId, programId, from, to, subject, textMessage, htmlMessage)

          session.prepareR(af.fragment.command).use(_.execute(af.argument).void).as(Result.unit)
        }

        def sendToProvider: F[Result[EmailId]] = {
          val request = Request[F](
            method = Method.POST,
            uri = config.sendMessageUri,
            headers = Headers(authHeader)
          ).withEntity(
            UrlForm(
              "from"    -> from.value.value,
              "to"      -> to.value.value,
              "subject" -> subject.value,
              "text"    -> textMessage.value,
            ).updateFormField("html", htmlMessage.map(_.value))
          )

          client.expect[SendResponse](request)
            .map(r => Result(r.id))
            .recover {
              case u @ UnexpectedStatus(status, _, _) =>
                OdbError.EmailSendError(s"Unexpected status '${u.status}' while attempting to send email.".some).asFailure
            }
        }

        (for {
          id <- ResultT(sendToProvider)
          _  <- ResultT(insertEmail(id))
        } yield id).value
      }
    }
  }

  object Statements:
    def insertEmail(
      emailId: EmailId,
      programId: Program.Id,
      from: EmailAddress,
      to: EmailAddress,
      subject: NonEmptyString,
      textMessage: NonEmptyString,
      htmlMessage: Option[NonEmptyString],
    ): AppliedFragment = 
      sql"""
        insert into t_email (
          c_email_id,
          c_program_id,
          c_sender_email,
          c_recipient_email,
          c_subject,
          c_text_message,
          c_html_message,
          c_status
        )
        select
          $email_id,
          $program_id,
          $email_address,
          $email_address,
          $text_nonempty,
          $text_nonempty,
          ${text_nonempty.opt},
          $email_status
      """.apply(
        emailId,
        programId,
        from,
        to,
        subject,
        textMessage,
        htmlMessage,
        EmailStatus.Queued
      )

    def updateStatus(emailId: EmailId, status: EmailStatus, timestamp: Timestamp): AppliedFragment =
      sql"""
        update t_email
        set c_status = $email_status,
            c_status_time = $core_timestamp
        where c_email_id = $email_id
      """.apply(status, timestamp, emailId)

}
