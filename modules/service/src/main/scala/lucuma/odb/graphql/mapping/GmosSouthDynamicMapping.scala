// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.GmosDynamicTables

trait GmosSouthDynamicMapping[F[_]] extends GmosDynamicTables[F] {

  lazy val GmosSouthDynamicMapping: ObjectMapping =
    ObjectMapping(
      tpe = GmosSouthDynamicType,
      fieldMappings = List(
        SqlField("id",       GmosSouthDynamicTable.Id, key = true),
        SqlField("exposure", GmosSouthDynamicTable.ExposureTime),



        SqlField("dtax",     GmosSouthDynamicTable.Dtax),
        SqlField("roi",      GmosSouthDynamicTable.Roi),

      )
    )

}

/*
    val Xbin: ColumnRef                   = col("c_xbin",                       gmos_x_binning)
    val Ybin: ColumnRef                   = col("c_ybin",                       gmos_y_binning)
    val AmpCount: ColumnRef               = col("c_amp_count",                  gmos_amp_count)
    val AmpGain: ColumnRef                = col("c_amp_gain",                   gmos_amp_gain)
    val AmpReadMode: ColumnRef            = col("c_amp_read_mode",              gmos_amp_read_mode)

  # GMOS exposure time
  exposure: TimeSpan!

  # GMOS CCD Readout
  readout: GmosCcdMode!

  # GMOS detector x offset
  dtax: GmosDtax!

  # GMOS region of interest
  roi: GmosRoi!

  # GMOS North grating
  gratingConfig: GmosNorthGratingConfig

  # GMOS North filter
  filter: GmosNorthFilter

  # GMOS North FPU
  fpu: GmosNorthFpu
 */
