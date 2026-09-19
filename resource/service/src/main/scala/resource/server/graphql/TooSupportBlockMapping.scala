// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import grackle.skunk.SkunkMapping
import resource.server.graphql.table.*

trait TooSupportBlockMapping[F[_]] extends ResourceBlockTables[F] with TimestampIntervalMapping[F]:
  this: SkunkMapping[F] =>

  lazy val TooSupportBlockMappings: List[TypeMapping] =
    List(
      ObjectMapping(TooSupportBlockType)(
        SqlField("_id", TooSupportBlockTable.Id, key = true, hidden = true),
        SqlField("site", TooSupportBlockTable.Site),
        SqlField("_start", TooSupportBlockTable.Start, hidden = true),
        SqlField("_end", TooSupportBlockTable.End, hidden = true),
        intervalField,
        SqlField("note", TooSupportBlockTable.Note),
        SqlField("tooSupport", TooSupportBlockTable.TooSupport)
      )
    )
