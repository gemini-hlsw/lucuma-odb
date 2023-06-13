// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.GmosDynamicTables

trait GmosCcdModeMapping[F[_]] extends GmosDynamicTables[F] {

  lazy val GmosCcdModeMapping: ObjectMapping =
    ObjectMapping(
      tpe = GmosCcdModeType,
      fieldMappings = List(

      )

    )

}

/*
    val Xbin: ColumnRef                   = col("c_xbin",                       gmos_x_binning)
    val Ybin: ColumnRef                   = col("c_ybin",                       gmos_y_binning)
    val AmpCount: ColumnRef               = col("c_amp_count",                  gmos_amp_count)
    val AmpGain: ColumnRef                = col("c_amp_gain",                   gmos_amp_gain)
    val AmpReadMode: ColumnRef            = col("c_amp_read_mode",              gmos_amp_read_mode)

  # GMOS X-binning
  xBin: GmosXBinning!

  # GMOS Y-binning
  yBin: GmosYBinning!

  # GMOS Amp Count
  ampCount: GmosAmpCount!

  # GMOS Amp Gain
  ampGain: GmosAmpGain!

  # GMOS Amp Read Mode
  ampReadMode: GmosAmpReadMode!
*/