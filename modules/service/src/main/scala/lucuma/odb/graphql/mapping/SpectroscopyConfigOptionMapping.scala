// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.SpectroscopyConfigOptionTable

trait SpectroscopyConfigOptionMapping[F[_]] extends SpectroscopyConfigOptionTable[F] {

  lazy val SpectroscopyConfigOptionMapping: ObjectMapping =
    ObjectMapping(
      tpe           = SpectroscopyConfigOptionType,
      fieldMappings = List(
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
          SpectroscopyConfigOptionTable.Instrument -> SpectrsocopyConfigOptionGmosNorthTable.Instrument,
          SpectroscopyConfigOptionTable.Index      -> SpectrsocopyConfigOptionGmosNorthTable.Index
        ))),

        SqlObject("gmosSouth", Join(List(
          SpectroscopyConfigOptionTable.Instrument -> SpectrsocopyConfigOptionGmosSouthTable.Instrument,
          SpectroscopyConfigOptionTable.Index      -> SpectrsocopyConfigOptionGmosSouthTable.Index
        )))
      )
    )

  lazy val SpectroscopyConfigOptionGmosNorthMapping: ObjectMapping =
    ObjectMapping(
      tpe           = SpectroscopyConfigOptionGmosNorthType,
      fieldMappings = List(
        SqlField("instrument", SpectrsocopyConfigOptionGmosNorthTable.Instrument, key = true, hidden = true),
        SqlField("index",      SpectrsocopyConfigOptionGmosNorthTable.Index, key = true, hidden = true),

        SqlField("fpu",        SpectrsocopyConfigOptionGmosNorthTable.Fpu),
        SqlField("grating",    SpectrsocopyConfigOptionGmosNorthTable.Grating),
        SqlField("filter",     SpectrsocopyConfigOptionGmosNorthTable.Filter)
      )
    )

  lazy val SpectroscopyConfigOptionGmosSouthMapping: ObjectMapping =
    ObjectMapping(
      tpe           = SpectroscopyConfigOptionGmosSouthType,
      fieldMappings = List(
        SqlField("instrument", SpectrsocopyConfigOptionGmosSouthTable.Instrument, key = true, hidden = true),
        SqlField("index",      SpectrsocopyConfigOptionGmosSouthTable.Index, key = true, hidden = true),

        SqlField("fpu",        SpectrsocopyConfigOptionGmosSouthTable.Fpu),
        SqlField("grating",    SpectrsocopyConfigOptionGmosSouthTable.Grating),
        SqlField("filter",     SpectrsocopyConfigOptionGmosSouthTable.Filter)
      )
    )

}
