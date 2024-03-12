// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.dataset_id
import lucuma.odb.util.Codecs.dataset_reference
import lucuma.odb.util.Codecs.int4_pos
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.observation_reference
import skunk.codec.text.text


trait DatasetReferenceView[F[_]] extends BaseMapping[F] {

  object DatasetReferenceView extends TableDef("v_dataset_reference") {
    val Id                   = col("c_dataset_id",            dataset_id)
    val ObservationId        = col("c_observation_id",        observation_id)
    val ObservationReference = col("c_observation_reference", observation_reference)
    val StepIndex            = col("c_step_index",            int4_pos)
    val ExposureIndex        = col("c_index",                 int4_pos)
    val DatasetReference     = col("c_dataset_reference",     dataset_reference)

    // Used in WhereDatasetReference
    val DatasetReferenceString = col("c_dataset_reference", text)
  }

}