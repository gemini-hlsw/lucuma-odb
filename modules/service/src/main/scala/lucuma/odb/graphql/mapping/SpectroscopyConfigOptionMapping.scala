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
        SqlField("instrument", SpectroscopyConfigOptionTable.Instrument, key = true),
        SqlField("index",      SpectroscopyConfigOptionTable.Index, key = true, hidden = true),
        SqlField("name",       SpectroscopyConfigOptionTable.Name),
        SqlField("focalPlane", SpectroscopyConfigOptionTable.FocalPlane),
        SqlField("fpuLabel",   SpectroscopyConfigOptionTable.FpuLabel),
        SqlObject("slitWidth"),
        SqlObject("slitLength"),
        SqlField("resolution", SpectroscopyConfigOptionTable.Resolution),
        SqlField("capability", SpectroscopyConfigOptionTable.Capability),

        SqlObject("gmosNorth", Join(List(
          SpectroscopyConfigOptionTable.Instrument -> SpectrsocopyConfigOptionGmosNorthTable.Instrument,
          SpectroscopyConfigOptionTable.Index      -> SpectrsocopyConfigOptionGmosNorthTable.Index
        )))
      )
    )

  lazy val SpectroscopyConfigOptionGmosNorthMapping: ObjectMapping =
    ObjectMapping(
      tpe           = SpectroscopyConfigOptionGmosNorthType,
      fieldMappings = List(
        SqlField("instrument", SpectrsocopyConfigOptionGmosNorthTable.Instrument, key = true),
        SqlField("index",      SpectrsocopyConfigOptionGmosNorthTable.Index, key = true, hidden = true),

        SqlField("fpu",        SpectrsocopyConfigOptionGmosNorthTable.Fpu),
        SqlField("grating",    SpectrsocopyConfigOptionGmosNorthTable.Grating),
        SqlField("filter",     SpectrsocopyConfigOptionGmosNorthTable.Filter)
      )
    )


/*
  name:               String!
  instrument:         Instrument!
  site:               Site!

  focalPlane:         FocalPlane!

  fpuLabel:           String! # e.g., '0.25"'
  slitWidth:          Angle!
  slitLength:         Angle!

  disperserLabel:     String! # e.g., 'B1200'
  filterLabel:        String! # e.g., 'JH'
  wavelengthMin:      Angle!
  wavelengthMax:      Angle!
  wavelengthOptimal:  Angle!
  wavelengthCoverage: Angle!

  resolution:         PosInt!

  adaptiveOptics:     Boolean!

  capability:         SpectroscopyCapabilities

  """
  For GMOS North options, the GMOS North configuration.  Null for other
  instruments.
  """
  gmosNorth: SpectroscopyConfigOptionGmosNorth

  """
  For GMOS South options, the GMOS South configuration.  Null for other
  instruments.
  """
  gmosSouth: SpectroscopyConfigOptionGmosSouth
 */
}
