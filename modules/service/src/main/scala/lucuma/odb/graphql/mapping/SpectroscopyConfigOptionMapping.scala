// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.SpectroscopyConfigOptionTable

trait SpectroscopyConfigOptionMapping[F[_]] extends SpectroscopyConfigOptionTable[F] {

  lazy val SpectroscopyConfigOptionMapping: ObjectMapping =
    ObjectMapping(SpectroscopyConfigOptionType)(

      SqlField("instrument",     SpectroscopyConfigOptionTable.Instrument, key = true),
      SqlField("index",          SpectroscopyConfigOptionTable.Index, key = true, hidden = true),
      SqlField("name",           SpectroscopyConfigOptionTable.Name),
      SqlField("focalPlane",     SpectroscopyConfigOptionTable.FocalPlane),

      SqlField("fpuLabel",       SpectroscopyConfigOptionTable.FpuLabel),
      SqlObject("slitWidth"),
      SqlObject("slitLength"),

      SqlField("disperserLabel", SpectroscopyConfigOptionTable.DisperserLabel),
      SqlField("filterLabel",    SpectroscopyConfigOptionTable.FilterLabel),

      SqlObject("wavelengthMin"),
      SqlObject("wavelengthMax"),
      SqlObject("wavelengthOptimal"),
      SqlObject("wavelengthCoverage"),

      SqlField("resolution",     SpectroscopyConfigOptionTable.Resolution),
      SqlField("adaptiveOptics", SpectroscopyConfigOptionTable.Ao),
      SqlField("capability",     SpectroscopyConfigOptionTable.Capability),
      SqlField("site",           SpectroscopyConfigOptionTable.Site),

      SqlObject("gmosNorth", Join(List(
        SpectroscopyConfigOptionTable.Instrument -> SpectroscopyConfigOptionGmosNorthTable.Instrument,
        SpectroscopyConfigOptionTable.Index      -> SpectroscopyConfigOptionGmosNorthTable.Index
      ))),

      SqlObject("gmosSouth", Join(List(
        SpectroscopyConfigOptionTable.Instrument -> SpectroscopyConfigOptionGmosSouthTable.Instrument,
        SpectroscopyConfigOptionTable.Index      -> SpectroscopyConfigOptionGmosSouthTable.Index
      ))),

      SqlObject("flamingos2", Join(List(
        SpectroscopyConfigOptionTable.Instrument -> SpectroscopyConfigOptionF2Table.Instrument,
        SpectroscopyConfigOptionTable.Index      -> SpectroscopyConfigOptionF2Table.Index
      )))

    )

  lazy val SpectroscopyConfigOptionGmosNorthMapping: ObjectMapping =
    ObjectMapping(SpectroscopyConfigOptionGmosNorthType)(

      SqlField("instrument", SpectroscopyConfigOptionGmosNorthTable.Instrument, key = true, hidden = true),
      SqlField("index",      SpectroscopyConfigOptionGmosNorthTable.Index, key = true, hidden = true),

      SqlField("fpu",        SpectroscopyConfigOptionGmosNorthTable.Fpu),
      SqlField("grating",    SpectroscopyConfigOptionGmosNorthTable.Grating),
      SqlField("filter",     SpectroscopyConfigOptionGmosNorthTable.Filter)

    )

  lazy val SpectroscopyConfigOptionGmosSouthMapping: ObjectMapping =
    ObjectMapping(SpectroscopyConfigOptionGmosSouthType)(

      SqlField("instrument", SpectroscopyConfigOptionGmosSouthTable.Instrument, key = true, hidden = true),
      SqlField("index",      SpectroscopyConfigOptionGmosSouthTable.Index, key = true, hidden = true),

      SqlField("fpu",        SpectroscopyConfigOptionGmosSouthTable.Fpu),
      SqlField("grating",    SpectroscopyConfigOptionGmosSouthTable.Grating),
      SqlField("filter",     SpectroscopyConfigOptionGmosSouthTable.Filter)

    )

  lazy val SpectroscopyConfigOptionF2Mapping: ObjectMapping =
    ObjectMapping(SpectroscopyConfigOptionF2Type)(

      SqlField("instrument", SpectroscopyConfigOptionF2Table.Instrument, key = true, hidden = true),
      SqlField("index",      SpectroscopyConfigOptionF2Table.Index, key = true, hidden = true),

      SqlField("fpu",        SpectroscopyConfigOptionF2Table.Fpu),
      SqlField("disperser",  SpectroscopyConfigOptionF2Table.Disperser),
      SqlField("filter",     SpectroscopyConfigOptionF2Table.Filter)

    )
}
