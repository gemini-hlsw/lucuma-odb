// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import skunk.codec.boolean.bool

trait ProgramNoteTable[F[_]] extends BaseMapping[F]:

  object ProgramNoteTable extends TableDef("t_program_note"):
    val Id                   = col("c_program_note_id", program_note_id)
    val ProgramId            = col("c_program_id", program_id)
    val Existence            = col("c_existence", existence)

    val Title                = col("c_title", text_nonempty)
    val Text                 = col("c_text", text_nonempty.opt)
    val Private              = col("c_private", bool)