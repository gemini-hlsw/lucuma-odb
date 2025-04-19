// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import lucuma.core.enums.EmailStatus
import lucuma.core.util.Timestamp
import lucuma.odb.data.EmailId
import lucuma.odb.util.Codecs.*
import skunk.AppliedFragment
import skunk.Session
import skunk.implicits.*

trait EmailWebhookService[F[Unit]] {
  
  def updateStatus(emailId: EmailId, status: EmailStatus, timestamp: Timestamp): F[Unit]
}

object EmailWebhookService:
  def fromSession[F[_]: MonadCancelThrow](session: Session[F]): EmailWebhookService[F] = 
    new EmailWebhookService[F]:
      override def updateStatus(emailId: EmailId, status: EmailStatus, timestamp: Timestamp): F[Unit] =
        val af = Statements.updateStatus(emailId, status, timestamp)
    
        session.prepareR(af.fragment.command)
          .use(_.execute(af.argument).void)
  
  object Statements:
    def updateStatus(emailId: EmailId, status: EmailStatus, timestamp: Timestamp): AppliedFragment =
      sql"""
        update t_email
        set c_status = $email_status,
            c_status_time = $core_timestamp
        where c_email_id = $email_id
      """.apply(status, timestamp, emailId)
