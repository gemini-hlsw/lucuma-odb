// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import grackle.skunk.SkunkMapping
import resource.server.graphql.table.*

trait TelescopeModeBlockMapping[F[_]]
    extends ResourceBlockTables[F]
    with TimestampIntervalMapping[F]:
  this: SkunkMapping[F] =>

  lazy val TelescopeModeBlockMappings: List[TypeMapping] =
    List(
      ObjectMapping(TelescopeModeBlockType)(
        SqlField("_id", TelescopeModeBlockTable.Id, key = true, hidden = true),
        SqlField("site", TelescopeModeBlockTable.Site),
        SqlField("_start", TelescopeModeBlockTable.Start, hidden = true),
        SqlField("_end", TelescopeModeBlockTable.End, hidden = true),
        intervalField,
        SqlField("note", TelescopeModeBlockTable.Note),
        SqlField("mode", TelescopeModeBlockTable.Mode),
        SqlField("programReferences", TelescopeModeBlockTable.ProgramReferences),
        SqlField("partner", TelescopeModeBlockTable.Partner)
      )
    )
