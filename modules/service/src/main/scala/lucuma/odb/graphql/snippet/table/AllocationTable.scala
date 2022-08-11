// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package table

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._
import skunk.codec.all._

trait AllocationTable[F[_]] { self: SkunkMapping[F] =>

  object AllocationTable extends TableDef("t_allocation") {
    val ProgramId = col("c_program_id", program_id)
    val Partner = col("c_partner", tag)
    val Duration = col("c_duration", interval)
  }

}