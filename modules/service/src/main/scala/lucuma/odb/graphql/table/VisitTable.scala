// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

 package lucuma.odb.graphql.table

 import lucuma.odb.graphql.BaseMapping
 import lucuma.odb.util.Codecs.core_timestamp
 import lucuma.odb.util.Codecs.instrument
 import lucuma.odb.util.Codecs.observation_id
 import lucuma.odb.util.Codecs.time_span
 import lucuma.odb.util.Codecs.visit_id


 trait VisitTable[F[_]] extends BaseMapping[F] {

   object VisitTable extends TableDef("t_visit") {
     val Id: ColumnRef            = col("c_visit_id",       visit_id)
     val ObservationId: ColumnRef = col("c_observation_id", observation_id)
     val Instrument: ColumnRef    = col("c_instrument",     instrument)
     val Created: ColumnRef       = col("c_created",        core_timestamp)

     object Raw {
       val NonChargedTime = col("c_raw_non_charged_time", time_span)
       val ProgramTime    = col("c_raw_program_time",     time_span)
     }

     object Final {
       val NonChargedTime = col("c_final_non_charged_time", time_span)
       val ProgramTime    = col("c_final_program_time",     time_span)
     }
   }

 }
