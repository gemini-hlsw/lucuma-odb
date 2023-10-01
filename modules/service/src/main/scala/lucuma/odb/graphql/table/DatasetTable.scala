// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.dataset_qa_state
import lucuma.odb.util.Codecs.int2_pos
import lucuma.odb.util.Codecs.int4_pos
import lucuma.odb.util.Codecs.site
import lucuma.odb.util.Codecs.step_id
import skunk.codec.temporal.date
import skunk.codec.text.varchar


trait DatasetTable[F[_]] extends BaseMapping[F] {

  object DatasetTable extends TableDef("t_dataset") {

    object DatasetId {
      val StepId: ColumnRef    = col("c_step_id", step_id)
      val Index: ColumnRef     = col("c_index",   int2_pos)
    }

    object File {
      val Site: ColumnRef  = col("c_file_site",  site)
      val Date: ColumnRef  = col("c_file_date",  date)
      val Index: ColumnRef = col("c_file_index", int4_pos)
      val Name: ColumnRef  = col("c_filename",   varchar)
    }

    val QaState: ColumnRef     = col("c_qa_state", dataset_qa_state.opt)

    object Time {
      val Start: ColumnRef     = col("c_start_time", core_timestamp.opt)
      val End: ColumnRef       = col("c_start_time", core_timestamp.opt)
    }
  }

}
