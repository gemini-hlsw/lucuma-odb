// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.odb.data.Nullable
import lucuma.odb.data.Tag
import lucuma.odb.graphql.input.ProposalAttachmentPropertiesInput
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import Services.Syntax.*

trait ProposalAttachmentMetadataService[F[_]] {
  def updateProposalAttachments(
    SET: ProposalAttachmentPropertiesInput.Edit,
    which: AppliedFragment
  )(using Transaction[F]): F[List[Tag]]
}

object ProposalAttachmentMetadataService {

  def instantiate[F[_]: Concurrent: Trace](using Services[F]): ProposalAttachmentMetadataService[F] =
    new ProposalAttachmentMetadataService[F] {
      def updateProposalAttachments(
          SET: ProposalAttachmentPropertiesInput.Edit,
          which: AppliedFragment
      )(using Transaction[F]): F[List[Tag]] =
        Statements.updateProposalAttachments(SET, which).fold(Nil.pure[F]) { af =>
          session.prepareR(af.fragment.query(tag)).use { pq =>
            pq.stream(af.argument, chunkSize = 1024).compile.toList
          }
        }
    }

  object Statements {

    def updates(SET: ProposalAttachmentPropertiesInput.Edit): Option[NonEmptyList[AppliedFragment]] = {
      val upDescription = sql"c_description = ${text_nonempty.opt}"
      val upChecked     = sql"c_checked = $bool"

      NonEmptyList.fromList(
        List(
          SET.description match {
            case Nullable.Null           => Some(upDescription(None))
            case Nullable.Absent         => None
            case Nullable.NonNull(value) => Some(upDescription(Some(value)))
          },
          SET.checked.map(upChecked)
        ).flatten
      )
    }

    def updateProposalAttachments(
      SET: ProposalAttachmentPropertiesInput.Edit,
      which: AppliedFragment
    ): Option[AppliedFragment] =
      updates(SET).map {us =>
        void"UPDATE t_proposal_attachment "  |+|
        void"SET " |+| us.intercalate(void", ") |+| void" " |+|
        void"WHERE (t_proposal_attachment.c_attachment_type, t_proposal_attachment.c_program_id) IN (" |+| which |+| void") " |+|
        void"RETURNING t_proposal_attachment.c_attachment_type"
      }
  }
}
