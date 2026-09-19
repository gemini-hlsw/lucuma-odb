// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import grackle.skunk.SkunkMapping
import resource.server.graphql.table.*

trait TelescopeAvailabilityBlockMapping[F[_]]
    extends ResourceBlockTables[F]
    with TimestampIntervalMapping[F]:
  this: SkunkMapping[F] =>

  lazy val TelescopeAvailabilityBlockMappings: List[TypeMapping] =
    List(
      ObjectMapping(TelescopeAvailabilityBlockType)(
        SqlField("_id", TelescopeAvailabilityBlockTable.Id, key = true, hidden = true),
        SqlField("site", TelescopeAvailabilityBlockTable.Site),
        SqlField("_start", TelescopeAvailabilityBlockTable.Start, hidden = true),
        SqlField("_end", TelescopeAvailabilityBlockTable.End, hidden = true),
        intervalField,
        SqlField("note", TelescopeAvailabilityBlockTable.Note),
        SqlField("availability", TelescopeAvailabilityBlockTable.Availability),
        SqlField("port", TelescopeAvailabilityBlockTable.Port),
        SqlField("reason", TelescopeAvailabilityBlockTable.Reason)
      )
    )
