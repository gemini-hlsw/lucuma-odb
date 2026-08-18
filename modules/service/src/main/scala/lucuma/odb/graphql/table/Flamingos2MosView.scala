// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import skunk.circe.codec.json.*
import skunk.codec.all.*

trait Flamingos2MosView[F[_]] extends BaseMapping[F]:

  object Flamingos2MosView extends TableDef("v_flamingos_2_mos"):

    val ObservationId: ColumnRef      = col("c_observation_id", observation_id)

    val Disperser: ColumnRef          = col("c_disperser", flamingos_2_disperser)
    val Filter: ColumnRef             = col("c_filter", flamingos_2_filter)

    val SlitWidth: ColumnRef          = col("c_slit_width", flamingos_2_custom_slit_width)
    val MaskAttachmentId: ColumnRef   = col("c_mask_attachment_id", attachment_id.opt)

    val ReadMode: ColumnRef           = col("c_read_mode", flamingos_2_read_mode.opt)
    val Reads: ColumnRef              = col("c_reads", flamingos_2_reads.opt)

    val Decker: ColumnRef             = col("c_decker", flamingos_2_decker.opt)
    val DeckerDefault: ColumnRef      = col("c_decker_default", flamingos_2_decker)

    val ReadoutMode: ColumnRef        = col("c_readout_mode", flamingos_2_readout_mode.opt)
    val ReadoutModeDefault: ColumnRef = col("c_readout_mode_default", flamingos_2_readout_mode)

    val OffsetPreset: ColumnRef       = col("c_mos_offset_preset", flamingos_2_mos_offset_preset)

    val SlitOffsetMode: ColumnRef          = col("c_slit_offset_mode", slit_offset_mode.opt)
    val SlitOffsetModeDefault: ColumnRef   = col("c_slit_offset_mode_default", slit_offset_mode.opt)
    val SlitOffsetModeEffective: ColumnRef = col("c_slit_offset_mode_effective", slit_offset_mode.opt)

    val TelescopeConfigs: ColumnRef          = col("c_telescope_configs", text.opt)
    val TelescopeConfigsDefault: ColumnRef   = col("c_telescope_configs_default", text)
    val TelescopeConfigsEffective: ColumnRef = col("c_telescope_configs_effective", text)

    val TelluricType: ColumnRef       = col("c_telluric_type", jsonb)

    val InitialDisperser: ColumnRef   = col("c_initial_disperser", flamingos_2_disperser)
    val InitialFilter: ColumnRef      = col("c_initial_filter", flamingos_2_filter)
    val InitialSlitWidth: ColumnRef   = col("c_initial_slit_width", flamingos_2_custom_slit_width)
