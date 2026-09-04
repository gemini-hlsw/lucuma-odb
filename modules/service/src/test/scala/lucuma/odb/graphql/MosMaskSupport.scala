// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.syntax.eq.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Attachment
import lucuma.core.model.Program
import lucuma.odb.util.Codecs.*
import skunk.Query
import skunk.circe.codec.all.*
import skunk.codec.all.*
import skunk.syntax.all.*

/**
 * Attachment test setup shared by the MOS suites.
 *
 * Rows are inserted straight into the database rather than through the file
 * service and S3, so the setup depends only on the database.
 *
 * The mask definition written here is a stub carrying only the instrument,
 * which is all the schema reads: the instrument column is generated from that
 * one key.  Suites that need a real design upload a real file instead.
 */
trait MosMaskSupport:
  self: OdbSuite =>

  protected def insertMosMaskAttachment(
    pid:            Program.Id,
    fileName:       String,
    maskInstrument: Instrument
  ): IO[Attachment.Id] =
    val q: Query[(Program.Id, String, String, Json), Attachment.Id] =
      sql"""
        INSERT INTO t_attachment (
          c_program_id,
          c_attachment_type,
          c_file_name,
          c_file_size,
          c_remote_path,
          c_mask_name,
          c_mask_definition
        )
        VALUES ($program_id, 'mos_mask', $text, 42, 'unused', $text, $jsonb)
        RETURNING c_attachment_id
      """.query(attachment_id)
    withSession(_.unique(q)(pid, fileName, maskName(fileName), stubDefinition(maskInstrument)))

  /** A non-mask observation attachment, which carries no instrument. */
  protected def insertObsAttachment(
    pid:      Program.Id,
    tpe:      String,
    fileName: String
  ): IO[Attachment.Id] =
    require(tpe =!= "mos_mask", "Use insertMosMaskAttachment for MOS masks.")
    val q: Query[(Program.Id, String, String), Attachment.Id] =
      sql"""
        INSERT INTO t_attachment (
          c_program_id,
          c_attachment_type,
          c_file_name,
          c_file_size,
          c_remote_path
        )
        VALUES ($program_id, $text::e_attachment_type, $text, 42, 'unused')
        RETURNING c_attachment_id
      """.query(attachment_id)
    withSession(_.unique(q)(pid, tpe, fileName))

  private def maskName(fileName: String): String =
    fileName.stripSuffix("_ODF.fits").toUpperCase

  private def stubDefinition(instrument: Instrument): Json =
    Json.obj("instrument" -> instrument.asJson)
