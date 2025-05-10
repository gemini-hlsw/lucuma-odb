// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

 import lucuma.odb.graphql.BaseMapping
 import lucuma.odb.util.Codecs.instrument
 import lucuma.odb.util.Codecs.mos_pre_imaging
 import lucuma.odb.util.Codecs.observation_id
 import lucuma.odb.util.Codecs.visit_id
 import skunk.codec.boolean.bool
 import skunk.codec.numeric.int8

trait Flamingos2StaticTable[F[_]] extends BaseMapping[F]:

  object Flamingos2StaticTable extends TableDef("t_flamingos_2_static"):
    val Id: ColumnRef             = col("c_static_id",       int8)
    val ObservationId: ColumnRef  = col("c_observation_id",  observation_id)
    val Instrument: ColumnRef     = col("c_instrument",      instrument)
    val VisitId: ColumnRef        = col("c_visit_id",        visit_id.opt)
    val MosPreImaging: ColumnRef  = col("c_mos_pre_imaging", mos_pre_imaging)
    val UseEOffsetting: ColumnRef = col("c_use_eoffsetting", bool)
