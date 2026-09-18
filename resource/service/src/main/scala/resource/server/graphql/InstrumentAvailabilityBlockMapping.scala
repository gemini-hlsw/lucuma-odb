// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import grackle.skunk.SkunkMapping
import resource.server.graphql.table.*

trait InstrumentAvailabilityBlockMapping[F[_]]
    extends ResourceBlockTables[F]
    with TimestampIntervalMapping[F]:
  this: SkunkMapping[F] =>

  lazy val InstrumentAvailabilityBlockMappings: List[TypeMapping] =
    List(
      ObjectMapping(InstrumentAvailabilityBlockType)(
        SqlField("_id", InstrumentAvailabilityBlockTable.Id, key = true, hidden = true),
        SqlField("site", InstrumentAvailabilityBlockTable.Site),
        SqlField("_start", InstrumentAvailabilityBlockTable.Start, hidden = true),
        SqlField("_end", InstrumentAvailabilityBlockTable.End, hidden = true),
        intervalField,
        SqlField("note", InstrumentAvailabilityBlockTable.Note),
        SqlField("instrument", InstrumentAvailabilityBlockTable.Instrument),
        SqlField("publishedName", InstrumentAvailabilityBlockTable.PublishedName),
        SqlObject("location"),
        SqlField("usage", InstrumentAvailabilityBlockTable.Usage)
      ),
      ObjectMapping(InstrumentAvailabilityBlockType / "location")(
        SqlField("_id", InstrumentAvailabilityBlockTable.Id, key = true, hidden = true),
        SqlField("place", InstrumentAvailabilityBlockTable.Place),
        SqlField("port", InstrumentAvailabilityBlockTable.Port)
      )
    )
