// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*

trait ObsAttachmentAssignmentTable[F[_]] extends BaseMapping[F] {

  object ObsAttachmentAssignmentTable extends TableDef("t_obs_attachment_assignment") {
    val ProgramId: ColumnRef       = col("c_program_id", program_id)
    val ObservationId: ColumnRef   = col("c_observation_id", observation_id)
    val ObsAttachmentId: ColumnRef = col("c_obs_attachment_id", obs_attachment_id)
  }
}
