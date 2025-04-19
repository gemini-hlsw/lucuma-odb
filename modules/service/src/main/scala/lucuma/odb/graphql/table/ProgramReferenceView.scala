// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.instrument
import lucuma.odb.util.Codecs.int4_pos
import lucuma.odb.util.Codecs.program_id
import lucuma.odb.util.Codecs.program_reference
import lucuma.odb.util.Codecs.program_type
import lucuma.odb.util.Codecs.science_subtype
import lucuma.odb.util.Codecs.semester
import lucuma.odb.util.Codecs.text_nonempty
import skunk.codec.text.text

trait ProgramReferenceView[F[_]] extends BaseMapping[F] {

  object ProgramReferenceView extends TableDef("v_program_reference") {
    val Id                = col("c_program_id",         program_id)
    val Instrument        = col("c_instrument",         instrument)
    val LibraryDesciption = col("c_library_desc",       text_nonempty)
    val ProgramReference  = col("c_program_reference",  program_reference)
    val ProgramType       = col("c_program_type",       program_type)
    val ScienceSubtype    = col("c_science_subtype",    science_subtype)
    val Semester          = col("c_semester",           semester)
    val SemesterIndex     = col("c_semester_index",     int4_pos)

    // Used in WhereProgramReference
    val ProgramReferenceString = col("c_program_reference", text)
  }

}
