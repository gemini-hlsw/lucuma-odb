// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.core.model.Attachment
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.input.AttachmentPropertiesInput
import lucuma.odb.graphql.input.AttachmentPropertiesInput.Edit
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import Services.Syntax.*

trait AttachmentMetadataService [F[_]] {
  def updateAttachments(
    SET: AttachmentPropertiesInput.Edit,
    which: AppliedFragment
  )(using Transaction[F]): F[List[Attachment.Id]]
}

object AttachmentMetadataService {

  def instantiate[F[_]: Concurrent: Trace](using Services[F]): AttachmentMetadataService[F] =
    new AttachmentMetadataService[F] {
      def updateAttachments(SET: Edit, which: AppliedFragment)(using Transaction[F]): F[List[Attachment.Id]] =
        Statements.updateAttachments(SET, which).fold(Nil.pure[F]) { af =>
          session.prepareR(af.fragment.query(attachment_id)).use { pq =>
            pq.stream(af.argument, chunkSize = 1024).compile.toList
          }
        }
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
  }
}
