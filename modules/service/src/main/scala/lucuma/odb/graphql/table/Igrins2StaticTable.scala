// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.instrument
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.visit_id
import lucuma.odb.util.Igrins2Codecs.igrins_2_offset_mode
import skunk.codec.boolean.bool
import skunk.codec.numeric.int8

trait Igrins2StaticTable[F[_]] extends BaseMapping[F]:

  object Igrins2StaticTable extends TableDef("t_igrins_2_static"):
    val Id: ColumnRef            = col("c_static_id",       int8)
    val ObservationId: ColumnRef = col("c_observation_id",  observation_id)
    val Instrument: ColumnRef    = col("c_instrument",      instrument)
    val VisitId: ColumnRef       = col("c_visit_id",        visit_id.opt)
    val SaveSVCImages: ColumnRef = col("c_save_svc_images", bool)
    val OffsetMode: ColumnRef    = col("c_offset_mode",     igrins_2_offset_mode)
