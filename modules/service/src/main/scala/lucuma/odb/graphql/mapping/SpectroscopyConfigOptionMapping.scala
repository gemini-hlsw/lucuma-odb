// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.SpectroscopyConfigOptionTable

trait SpectroscopyConfigOptionMapping[F[_]] extends SpectroscopyConfigOptionTable[F]:

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

      SqlObject("flamingos2", Join(List(
        SpectroscopyConfigOptionTable.Instrument -> SpectroscopyConfigOptionFlamingos2Table.Instrument,
        SpectroscopyConfigOptionTable.Index      -> SpectroscopyConfigOptionFlamingos2Table.Index
      ))),

      SqlObject("ghost", Join(List(
        SpectroscopyConfigOptionTable.Instrument -> SpectroscopyConfigOptionGhostTable.Instrument,
        SpectroscopyConfigOptionTable.Index      -> SpectroscopyConfigOptionGhostTable.Index
      ))),

      SqlObject("gmosNorth", Join(List(
        SpectroscopyConfigOptionTable.Instrument -> SpectroscopyConfigOptionGmosNorthTable.Instrument,
        SpectroscopyConfigOptionTable.Index      -> SpectroscopyConfigOptionGmosNorthTable.Index
      ))),

      SqlObject("gmosSouth", Join(List(
        SpectroscopyConfigOptionTable.Instrument -> SpectroscopyConfigOptionGmosSouthTable.Instrument,
        SpectroscopyConfigOptionTable.Index      -> SpectroscopyConfigOptionGmosSouthTable.Index
      ))),

      SqlObject("gnirs", Join(List(
        SpectroscopyConfigOptionTable.Instrument -> SpectroscopyConfigOptionGnirsTable.Instrument,
        SpectroscopyConfigOptionTable.Index      -> SpectroscopyConfigOptionGnirsTable.Index
      )))

    )

  lazy val SpectroscopyConfigOptionF2Mapping: ObjectMapping =
    ObjectMapping(SpectroscopyConfigOptionFlamingos2Type)(
      SqlField("instrument", SpectroscopyConfigOptionFlamingos2Table.Instrument, key = true, hidden = true),
      SqlField("index",      SpectroscopyConfigOptionFlamingos2Table.Index, key = true, hidden = true),

      SqlField("fpu",        SpectroscopyConfigOptionFlamingos2Table.Fpu),
      SqlField("disperser",  SpectroscopyConfigOptionFlamingos2Table.Disperser),
      SqlField("filter",     SpectroscopyConfigOptionFlamingos2Table.Filter)
    )

  lazy val SpectroscopyConfigOptionGhostMapping: ObjectMapping =
    ObjectMapping(SpectroscopyConfigOptionGhostType)(
      SqlField("instrument", SpectroscopyConfigOptionGhostTable.Instrument, key = true, hidden = true),
      SqlField("index",      SpectroscopyConfigOptionGhostTable.Index, key = true, hidden = true),

      SqlField("binning",        SpectroscopyConfigOptionGhostTable.Binning),
      SqlField("resolutionMode", SpectroscopyConfigOptionGhostTable.ResolutionMode)
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

  lazy val SpectroscopyConfigOptionGnirsMapping: ObjectMapping =
    ObjectMapping(SpectroscopyConfigOptionGnirsType)(
      SqlField("instrument", SpectroscopyConfigOptionGnirsTable.Instrument, key = true, hidden = true),
      SqlField("index",      SpectroscopyConfigOptionGnirsTable.Index, key = true, hidden = true),

      SqlField("grating",    SpectroscopyConfigOptionGnirsTable.Grating),
      SqlField("filter",     SpectroscopyConfigOptionGnirsTable.Filter),
      SqlField("fpu",        SpectroscopyConfigOptionGnirsTable.Fpu)
    )