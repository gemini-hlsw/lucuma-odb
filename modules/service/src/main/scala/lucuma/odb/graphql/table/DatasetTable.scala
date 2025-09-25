// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.dataset_id
import lucuma.odb.util.Codecs.dataset_qa_state
import lucuma.odb.util.Codecs.idempotency_key
import lucuma.odb.util.Codecs.int4_pos
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.observation_reference
import lucuma.odb.util.Codecs.site
import lucuma.odb.util.Codecs.step_id
import lucuma.odb.util.Codecs.text_nonempty
import lucuma.odb.util.Codecs.varchar_nonempty
import lucuma.odb.util.Codecs.visit_id
import skunk.codec.temporal.date


trait DatasetTable[F[_]] extends BaseMapping[F] {

  object DatasetTable extends TableDef("t_dataset") {

    val Id: ColumnRef            = col("c_dataset_id",            dataset_id)
    val StepId: ColumnRef        = col("c_step_id",               step_id)
    val ObservationReference     = col("c_observation_reference", observation_reference)
    val StepIndex: ColumnRef     = col("c_step_index",            int4_pos)
    val ExposureIndex: ColumnRef = col("c_exposure_index",        int4_pos)
    val ObservationId: ColumnRef = col("c_observation_id",        observation_id)
    val VisitId: ColumnRef       = col("c_visit_id",              visit_id)

    object File {
      val Site: ColumnRef  = col("c_file_site",  site)
      val Date: ColumnRef  = col("c_file_date",  date)
      val Index: ColumnRef = col("c_file_index", int4_pos)
      val Name: ColumnRef  = col("c_filename",   varchar_nonempty)
    }

    val QaState: ColumnRef        = col("c_qa_state",        dataset_qa_state.opt)
    val Comment: ColumnRef        = col("c_comment",         text_nonempty.opt)
    val IdempotencyKey: ColumnRef = col("c_idempotency_key", idempotency_key.opt)

    object Time {
      val Start: ColumnRef     = col("c_start_time", core_timestamp.opt)
      val End: ColumnRef       = col("c_end_time",   core_timestamp.opt)
    }
  }

}
