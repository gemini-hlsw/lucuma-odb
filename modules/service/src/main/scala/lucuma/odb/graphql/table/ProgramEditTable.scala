// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.all.*

trait ProgramEditTable[F[_]] extends BaseMapping[F] {

  // TODO: is this used for anything?
  object ProgramEditTable extends TableDef("t_program_event") {
    val EventId   = col("c_event_id", int8)
    val EditType  = col("c_edit_type", edit_type)
    val ProgramId = col("c_program_id", program_id)
  }

}