// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import grackle.skunk.SkunkMapping
import resource.server.graphql.table.*

trait TelescopeSubsystemBlockMapping[F[_]]
    extends ResourceBlockTables[F]
    with TimestampIntervalMapping[F]:
  this: SkunkMapping[F] =>

  lazy val TelescopeSubsystemBlockMappings: List[TypeMapping] =
    List(
      ObjectMapping(TelescopeSubsystemAvailabilityBlockType)(
        SqlField("_id", TelescopeSubsystemBlockTable.Id, key = true, hidden = true),
        SqlField("site", TelescopeSubsystemBlockTable.Site),
        SqlField("_start", TelescopeSubsystemBlockTable.Start, hidden = true),
        SqlField("_end", TelescopeSubsystemBlockTable.End, hidden = true),
        intervalField,
        SqlField("note", TelescopeSubsystemBlockTable.Note),
        SqlField("subsystem", TelescopeSubsystemBlockTable.Subsystem),
        SqlField("usage", TelescopeSubsystemBlockTable.Usage),
        SqlField("powerSource", TelescopeSubsystemBlockTable.PowerSource)
      )
    )
