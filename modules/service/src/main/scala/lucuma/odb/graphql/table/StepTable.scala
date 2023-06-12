// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

 package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.instrument
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.step_id
import lucuma.odb.util.Codecs.step_type
import lucuma.odb.util.Codecs.visit_id


trait StepTable[F[_]] extends BaseMapping[F] {

  object StepTable extends TableDef("t_step") {
    val Id: ColumnRef            = col("c_step_id",        step_id)
    val ObservationId: ColumnRef = col("c_observation_id", observation_id)
    val Instrument: ColumnRef    = col("c_instrument",     instrument)
    val VisitId: ColumnRef       = col("c_visit_id",       visit_id)
    val StepType: ColumnRef      = col("c_step_type",      step_type)
    val Created: ColumnRef       = col("c_created",        core_timestamp)
  }

}
