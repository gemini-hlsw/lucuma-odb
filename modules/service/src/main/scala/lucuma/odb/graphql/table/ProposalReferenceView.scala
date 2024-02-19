// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table
import lucuma.odb.util.Codecs.int4_pos
import lucuma.odb.util.Codecs.program_id
import lucuma.odb.util.Codecs.proposal_reference
import lucuma.odb.util.Codecs.semester

trait ProposalReferenceView[F[_]] extends BaseMapping[F] {

  object ProposalReferenceView extends TableDef("v_proposal_reference") {
    val Id                = col("c_program_id",         program_id)
    val Semester          = col("c_semester",           semester)
    val SemesterIndex     = col("c_semester_index",     int4_pos)
    val ProposalReference = col("c_proposal_reference", proposal_reference)
  }


}
