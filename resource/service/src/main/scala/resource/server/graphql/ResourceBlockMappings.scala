// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import grackle.skunk.SkunkMapping
import resource.server.graphql.table.*

/** The six block types the flat interval queries serve. */
trait ResourceBlockMappings[F[_]]
    extends TimestampIntervalMapping[F]
    with InstrumentComponentTables[F]:
  this: SkunkMapping[F] =>

  lazy val ResourceBlockMappings: List[TypeMapping] =
    List(
      blockMapping(TooSupportBlockType, TooSupportBlockTable)(
        SqlField("tooSupport", TooSupportBlockTable.TooSupport)
      ),
      blockMapping(TelescopeAvailabilityBlockType, TelescopeAvailabilityBlockTable)(
        SqlField("availability", TelescopeAvailabilityBlockTable.Availability),
        SqlField("port", TelescopeAvailabilityBlockTable.Port),
        SqlField("reason", TelescopeAvailabilityBlockTable.Reason)
      ),
      blockMapping(TelescopeModeBlockType, TelescopeModeBlockTable)(
        SqlField("mode", TelescopeModeBlockTable.Mode),
        SqlField("programReferences", TelescopeModeBlockTable.ProgramReferences),
        SqlField("partner", TelescopeModeBlockTable.Partner)
      ),
      blockMapping(InstrumentAvailabilityBlockType, InstrumentAvailabilityBlockTable)(
        SqlField("instrument", InstrumentAvailabilityBlockTable.Instrument),
        SqlField("publishedName", InstrumentAvailabilityBlockTable.PublishedName),
        SqlObject("location"),
        SqlField("usage", InstrumentAvailabilityBlockTable.Usage)
      ),
      ObjectMapping(InstrumentAvailabilityBlockType / "location")(
        SqlField(IdField, InstrumentAvailabilityBlockTable.Id, key = true, hidden = true),
        SqlField("place", InstrumentAvailabilityBlockTable.Place),
        SqlField("port", InstrumentAvailabilityBlockTable.Port)
      ),
      blockMapping(TelescopeSubsystemAvailabilityBlockType, TelescopeSubsystemBlockTable)(
        SqlField("subsystem", TelescopeSubsystemBlockTable.Subsystem),
        SqlField("usage", TelescopeSubsystemBlockTable.Usage),
        SqlField("powerSource", TelescopeSubsystemBlockTable.PowerSource)
      ),
      blockMapping(InstrumentComponentAvailabilityBlockType, InstrumentComponentBlockTable)(
        SqlObject(
          "component",
          Join(InstrumentComponentBlockTable.ComponentId, InstrumentComponentTable.Id)
        ),
        SqlField("usage", InstrumentComponentBlockTable.Usage),
        SqlField("location", InstrumentComponentBlockTable.Location)
      ),
      // A block's component comes from the catalog table, not from the per-site view the
      // `components` root reads: the view computes a distinct over every block at the site on
      // each query, and the block already knows its component's id.
      ObjectMapping(InstrumentComponentAvailabilityBlockType / "component")(
        componentFields(InstrumentComponentTable)*
      )
    )
