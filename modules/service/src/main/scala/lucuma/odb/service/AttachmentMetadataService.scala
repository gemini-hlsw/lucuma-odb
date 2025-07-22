// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.Attachment
import lucuma.core.util.Timestamp
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.input.AttachmentPropertiesInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import Services.Syntax.*

trait AttachmentMetadataService [F[_]] {

  def updateAttachments(
    input: AccessControl.Checked[AttachmentPropertiesInput.Edit]
  )(using Transaction[F]): F[Result[List[Attachment.Id]]]

  def getUpdatedAt(aids: NonEmptyList[Attachment.Id])(using NoTransaction[F], SuperUserAccess): F[Map[Attachment.Id, Timestamp]]
}

object AttachmentMetadataService {

  def instantiate[F[_]: Concurrent](using Services[F]): AttachmentMetadataService[F] =
    new AttachmentMetadataService[F] {

      override def updateAttachments(
        input: AccessControl.Checked[AttachmentPropertiesInput.Edit]
      )(using Transaction[F]): F[Result[List[Attachment.Id]]] =
        input.fold(Result(Nil).pure[F]): (SET, which) =>
          Statements.updateAttachments(SET, which).fold(Nil.pure[F]) { af =>
            session.prepareR(af.fragment.query(attachment_id)).use { pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList
            }
          }.map(Result.success)

      // Called by other services, no access validation is performed.
      def getUpdatedAt(aids: NonEmptyList[Attachment.Id])(using NoTransaction[F], SuperUserAccess): F[Map[Attachment.Id, Timestamp]] =
        val uniqueIds = aids.distinct
        session.execute(Statements.getUpdatedAt(uniqueIds))(uniqueIds.toList).map(_.toMap)
    }

  object Statements {

    def updates(SET: AttachmentPropertiesInput.Edit): Option[NonEmptyList[AppliedFragment]] = {
      val upDescription = sql"c_description = ${text_nonempty.opt}"
      val upChecked = sql"c_checked = $bool"
      NonEmptyList.fromList(
        List(
          SET.description match {
            case Nullable.Null => Some(upDescription(None))
            case Nullable.Absent => None
            case Nullable.NonNull(value) => Some(upDescription(Some(value)))
          },
          SET.checked.map(upChecked)
        ).flatten
      )
    }

    def updateAttachments(SET: AttachmentPropertiesInput.Edit, which: AppliedFragment): Option[AppliedFragment] =
      updates(SET).map { us =>
        void"UPDATE t_attachment "                                           |+|
        void"SET " |+| us.intercalate(void", ") |+| void" "                  |+|
        void"WHERE t_attachment.c_attachment_id IN (" |+| which |+| void") " |+|
        void"RETURNING t_attachment.c_attachment_id"
      }

    def getUpdatedAt(aids: NonEmptyList[Attachment.Id]): Query[List[Attachment.Id], (Attachment.Id, Timestamp)] =
      sql"""
        SELECT
          c_attachment_id,
          c_updated_at
        FROM t_attachment
        WHERE c_attachment_id IN(${attachment_id.list(aids.size)})
      """.query(attachment_id *: core_timestamp)
  }
}
