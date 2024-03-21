// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*

trait AsterismTargetTable[F[_]] extends BaseMapping[F] {

  object AsterismTargetTable extends TableDef("t_asterism_target") {

    val ProgramId: ColumnRef =
      col("c_program_id", program_id)

    val ObservationId: ColumnRef =
      col("c_observation_id", observation_id)

    val TargetId: ColumnRef =
      col("c_target_id", target_id)

  }

}
