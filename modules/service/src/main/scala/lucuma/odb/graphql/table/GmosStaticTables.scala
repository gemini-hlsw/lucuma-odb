// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

 import lucuma.odb.graphql.BaseMapping
 import lucuma.odb.util.Codecs.instrument
 import lucuma.odb.util.Codecs.mos_pre_imaging
 import lucuma.odb.util.Codecs.observation_id
 import lucuma.odb.util.Codecs.visit_id
 import lucuma.odb.util.GmosCodecs.gmos_north_detector
 import lucuma.odb.util.GmosCodecs.gmos_north_stage_mode
 import lucuma.odb.util.GmosCodecs.gmos_south_detector
 import lucuma.odb.util.GmosCodecs.gmos_south_stage_mode
 import skunk.codec.numeric.int8

 trait GmosStaticTables[F[_]] extends BaseMapping[F] {

   object GmosNorthStaticTable extends TableDef("t_gmos_north_static") {
     val Id: ColumnRef            = col("c_static_id",       int8)
     val ObservationId: ColumnRef = col("c_observation_id",  observation_id)
     val Instrument: ColumnRef    = col("c_instrument",      instrument)
     val VisitId: ColumnRef       = col("c_visit_id",        visit_id.opt)
     val Detector: ColumnRef      = col("c_detector",        gmos_north_detector)
     val MosPreImaging: ColumnRef = col("c_mos_pre_imaging", mos_pre_imaging)
     val StageMode: ColumnRef     = col("c_stage_mode",      gmos_north_stage_mode)
   }

   object GmosSouthStaticTable extends TableDef("t_gmos_south_static") {
     val Id: ColumnRef            = col("c_static_id",       int8)
     val ObservationId: ColumnRef = col("c_observation_id",  observation_id)
     val Instrument: ColumnRef    = col("c_instrument",      instrument)
     val VisitId: ColumnRef       = col("c_visit_id",        visit_id.opt)
     val Detector: ColumnRef      = col("c_detector",        gmos_south_detector)
     val MosPreImaging: ColumnRef = col("c_mos_pre_imaging", mos_pre_imaging)
     val StageMode: ColumnRef     = col("c_stage_mode",      gmos_south_stage_mode)
   }

 }
