// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._
import skunk.codec.all._

trait ProgramEditTable[F[_]] extends BaseMapping[F] {

  object ProgramEditTable extends TableDef("t_program_event") {
    val EventId   = col("c_event_id", int8)
    val EditType  = col("c_edit_type", edit_type)
    val ProgramId = col("c_program_id", program_id)
  }

}