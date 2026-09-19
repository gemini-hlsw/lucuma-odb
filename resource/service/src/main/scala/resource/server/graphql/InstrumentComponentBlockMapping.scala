// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import grackle.skunk.SkunkMapping
import resource.server.graphql.table.*

trait InstrumentComponentBlockMapping[F[_]]
    extends InstrumentComponentTables[F]
    with TimestampIntervalMapping[F]:
  this: SkunkMapping[F] =>

  lazy val InstrumentComponentBlockMappings: List[TypeMapping] =
    List(
      ObjectMapping(InstrumentComponentAvailabilityBlockType)(
        SqlField("_id", InstrumentComponentBlockTable.Id, key = true, hidden = true),
        SqlField("site", InstrumentComponentBlockTable.Site),
        SqlField("_start", InstrumentComponentBlockTable.Start, hidden = true),
        SqlField("_end", InstrumentComponentBlockTable.End, hidden = true),
        intervalField,
        SqlField("note", InstrumentComponentBlockTable.Note),
        SqlObject("component",
                  Join(InstrumentComponentBlockTable.ComponentId, InstrumentComponentTable.Id)
        ),
        SqlField("usage", InstrumentComponentBlockTable.Usage),
        SqlField("location", InstrumentComponentBlockTable.Location)
      ),
      // Bound to the path, not the type: the `components` root reaches the same
      // GraphQL type through a view instead. A type-level mapping would shadow it.
      ObjectMapping(InstrumentComponentAvailabilityBlockType / "component")(
        SqlField("id", InstrumentComponentTable.Id, key = true),
        SqlField("instrument", InstrumentComponentTable.Instrument),
        SqlField("componentType", InstrumentComponentTable.ComponentType),
        SqlField("code", InstrumentComponentTable.Code),
        SqlField("name", InstrumentComponentTable.Name),
        SqlField("barcode", InstrumentComponentTable.Barcode),
        SqlField("aliases", InstrumentComponentTable.Aliases),
        SqlField("existence", InstrumentComponentTable.Existence)
      )
    )
