// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._

trait AsterismTargetTable[F[_]] { self: SkunkMapping[F] =>

  object AsterismTargetTable extends TableDef("t_asterism_target") {

    val ProgramId: ColumnRef =
      col("c_program_id", program_id)

    val ObservationId: ColumnRef =
      col("c_observation_id", observation_id)

    val TargetId: ColumnRef =
      col("c_target_id", target_id)

  }

}
