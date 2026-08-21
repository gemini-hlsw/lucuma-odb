// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.Instrument
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
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

  /**
   * Refuses a MOS mask attachment cut for an instrument other than the one the
   * given observations use.
   *
   * The widened mask foreign key on each MOS mode table is what actually
   * guarantees this.  The lookup exists only so the common mistake -- picking
   * the wrong plate -- gets a message naming the plate and both instruments,
   * rather than the composite violation's list of four conditions.  A missing
   * attachment, one of the wrong type, and one belonging to another program are
   * all left to that violation, which is also why the lookup is scoped to the
   * observations' program: it must not report the mask name of a plate the
   * caller cannot see.
   */
  def validateMaskInstrument(
    attachmentId: Option[Attachment.Id],
    instrument:   Instrument,
    which:        List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]
}

object AttachmentMetadataService {

  def maskInstrumentMismatchMessage(
    maskName:       NonEmptyString,
    maskInstrument: Instrument,
    instrument:     Instrument
  ): String =
    s"Mask ${maskName.value} is designed for ${maskInstrument.longName}, but this observation uses ${instrument.longName}."

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

      override def validateMaskInstrument(
        attachmentId: Option[Attachment.Id],
        instrument:   Instrument,
        which:        List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        (attachmentId, NonEmptyList.fromList(which)) match
          case (Some(aid), Some(oids)) =>
            val af = Statements.selectMaskInstrument(aid, oids)
            session.prepareR(af.fragment.query(Statements.MaskNameAndInstrument)).use: pq =>
              pq.option(af.argument).map:
                case Some((maskName, maskInstrument)) if maskInstrument =!= instrument =>
                  Result.failure(maskInstrumentMismatchMessage(maskName, maskInstrument, instrument))
                case _                                                                =>
                  Result.unit
          case _                       =>
            Result.unit.pure[F]
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

    val MaskNameAndInstrument: Decoder[(NonEmptyString, Instrument)] =
      text_nonempty *: instrument

    // Joined through the program so a mask on another program simply yields no
    // row, leaving that case to the mask foreign key.
    def selectMaskInstrument(
      aid:   Attachment.Id,
      which: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT DISTINCT a.c_mask_name, a.c_mask_instrument
        FROM t_attachment a
        JOIN t_observation o ON o.c_program_id = a.c_program_id
        WHERE a.c_attachment_id   = $attachment_id
          AND a.c_attachment_type = 'mos_mask'
          AND o.c_observation_id IN (
      """.apply(aid)                                            |+|
      which.map(sql"$observation_id".apply).intercalate(void", ") |+|
      void")"

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
