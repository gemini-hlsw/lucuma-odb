// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.int4_pos
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.observation_reference
import lucuma.odb.util.Codecs.program_id
import skunk.codec.text.text

trait ObservationReferenceView[F[_]] extends BaseMapping[F] {

  object ObservationReferenceView extends TableDef("v_observation_reference") {
    val Id                   = col("c_observation_id",        observation_id)
    val ProgramId            = col("c_program_id",            program_id)
    val ObservationIndex     = col("c_observation_index",     int4_pos)
    val ObservationReference = col("c_observation_reference", observation_reference)

    // Used in WhereObservationReference
    val ObservationReferenceString = col("c_observation_reference", text)
  }

}
