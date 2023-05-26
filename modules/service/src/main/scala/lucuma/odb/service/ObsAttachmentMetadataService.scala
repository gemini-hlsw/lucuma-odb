// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import edu.gemini.grackle.Result
import lucuma.core.model.ObsAttachment
import lucuma.core.model.User
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.input.ObsAttachmentPropertiesInput
import lucuma.odb.graphql.input.ObsAttachmentPropertiesInput.Edit
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import Services.Syntax.*

trait ObsAttachmentMetadataService [F[_]] {
  def updateObsAttachments(
    SET: ObsAttachmentPropertiesInput.Edit,
    which: AppliedFragment
  )(using Transaction[F]): F[List[ObsAttachment.Id]]
}

object ObsAttachmentMetadataService {

  def instantiate[F[_]: Concurrent: Trace](using Services[F]): ObsAttachmentMetadataService[F] = 
    new ObsAttachmentMetadataService[F] {
      def updateObsAttachments(SET: Edit, which: AppliedFragment)(using Transaction[F]): F[List[ObsAttachment.Id]] = 
        Statements.updateObsAttachments(SET, which).fold(Nil.pure[F]) { af =>
          session.prepareR(af.fragment.query(obs_attachment_id)).use { pq =>
            pq.stream(af.argument, chunkSize = 1024).compile.toList
          }
        }
    }

  object Statements {

    def updates(SET: ObsAttachmentPropertiesInput.Edit): Option[NonEmptyList[AppliedFragment]] = {
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

    def updateObsAttachments(SET: ObsAttachmentPropertiesInput.Edit, which: AppliedFragment): Option[AppliedFragment] =
      updates(SET).map { us =>
        void"UPDATE t_obs_attachment "                                           |+|
        void"SET " |+| us.intercalate(void", ") |+| void" "                  |+|
        void"WHERE t_obs_attachment.c_obs_attachment_id IN (" |+| which |+| void") " |+|
        void"RETURNING t_obs_attachment.c_obs_attachment_id"
      }
  }
}
