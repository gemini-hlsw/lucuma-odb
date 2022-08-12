// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.Result
import edu.gemini.grackle.skunk.SkunkMapping

import java.util.concurrent.atomic.AtomicLong

import table.ProgramTable

trait ProgramEditMapping[F[_]]
  extends ProgramTable[F] { this: SkunkMapping[F] =>

  lazy val ProgramEditType = schema.ref("ProgramEdit")

  private val ProgramEditId = AtomicLong(0L)

  lazy val ProgramEditMapping =
    ObjectMapping(
      tpe = ProgramEditType,
      fieldMappings = List(
        SqlField("synthetic-key", ProgramTable.Id, key = true, hidden = true),
        // todo: id
        SqlObject("value"),
        // todo: editType
      )
    )

}

