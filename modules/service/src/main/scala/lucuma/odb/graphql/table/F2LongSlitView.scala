// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.F2Codecs.*
import skunk.circe.codec.json.*

trait F2LongSlitView[F[_]] extends BaseMapping[F]:

  object F2LongSlitView extends TableDef("v_flamingos_2_long_slit"):

    val Disperser: ColumnRef          = col("c_disperser", f2_disperser)
    val Filter: ColumnRef             = col("c_filter", f2_filter)
    val Fpu: ColumnRef                = col("c_fpu", f2_fpu)

    val ObservationId: ColumnRef      = col("c_observation_id", observation_id)

    val ReadMode: ColumnRef           = col("c_read_mode", f2_read_mode.opt)

    val Reads: ColumnRef              = col("c_reads", f2_reads.opt)

    val Decker: ColumnRef             = col("c_decker", f2_decker.opt)
    val DeckerDefault: ColumnRef      = col("c_decker_default", f2_decker)

    val ReadoutMode: ColumnRef        = col("c_readout_mode", f2_readout_mode.opt)
    val ReadoutModeDefault: ColumnRef = col("c_readout_mode_default", f2_readout_mode)

    val ImageQuality: ColumnRef       = col("c_image_quality", image_quality_preset)
    val SourceProfile: ColumnRef      = col("c_source_profile", jsonb.opt)

    val InitialDisperser: ColumnRef   = col("c_initial_disperser", f2_disperser)
    val InitialFilter: ColumnRef      = col("c_initial_filter", f2_filter)
    val InitialFpu: ColumnRef         = col("c_initial_fpu", f2_fpu)

