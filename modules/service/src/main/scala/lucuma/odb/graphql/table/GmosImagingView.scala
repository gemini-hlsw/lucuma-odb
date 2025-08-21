// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.codec.text.*

trait GmosImagingView[F[_]] extends BaseMapping[F]:

  object GmosNorthImagingView extends TableDef("v_gmos_north_imaging"):

    val ObservationId: ColumnRef                  = col("c_observation_id", observation_id)
    val ExplicitBin: ColumnRef                    = col("c_bin", gmos_binning.opt)
    val ExplicitAmpReadMode: ColumnRef            = col("c_amp_read_mode", gmos_amp_read_mode.opt)
    val ExplicitAmpGain: ColumnRef                = col("c_amp_gain", gmos_amp_gain.opt)
    val ExplicitRoi: ColumnRef                    = col("c_roi", gmos_roi.opt)
    val Offsets: ColumnRef                        = col("c_offsets", text)
    val Filters: ColumnRef                        = col("c_filters", _gmos_north_filter)
    val InitialFilters: ColumnRef                 = col("c_initial_filters", _gmos_north_filter)
    val ExplicitMultipleFiltersMode: ColumnRef    = col("c_multiple_filters_mode", multiple_filters_mode.opt)

  object GmosSouthImagingView extends TableDef("v_gmos_south_imaging"):

    val ObservationId: ColumnRef                  = col("c_observation_id", observation_id)
    val ExplicitBin: ColumnRef                    = col("c_bin", gmos_binning.opt)
    val ExplicitAmpReadMode: ColumnRef            = col("c_amp_read_mode", gmos_amp_read_mode.opt)
    val ExplicitAmpGain: ColumnRef                = col("c_amp_gain", gmos_amp_gain.opt)
    val ExplicitRoi: ColumnRef                    = col("c_roi", gmos_roi.opt)
    val Offsets: ColumnRef                        = col("c_offsets", text)
    val Filters: ColumnRef                        = col("c_filters", _gmos_south_filter)
    val InitialFilters: ColumnRef                 = col("c_initial_filters", _gmos_south_filter)
    val ExplicitMultipleFiltersMode: ColumnRef    = col("c_multiple_filters_mode", multiple_filters_mode.opt)
