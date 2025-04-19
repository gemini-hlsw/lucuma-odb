// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.dataset_id
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.site
import lucuma.odb.util.Codecs.time_charge_discount_type
import lucuma.odb.util.Codecs.time_span
import lucuma.odb.util.Codecs.visit_id
import skunk.codec.numeric.int8
import skunk.codec.text.text

trait TimeChargeDiscountTable[F[_]] extends BaseMapping[F]:

  object TimeChargeDiscountTable extends TableDef("t_time_charge_discount"):

    val Id           = col("c_id",               int8)
    val VisitId      = col("c_visit_id",         visit_id)
    val Start        = col("c_start",            core_timestamp)
    val End          = col("c_end",              core_timestamp)
    val Amount       = col("c_amount",           time_span)
    val Comment      = col("c_comment",          text)

    val DiscountType = col("c_type",             time_charge_discount_type)

    object Daylight:
      val Site       = col("c_site",             site)

  object TimeChargeDiscountDatasetTable extends TableDef("t_time_charge_discount_dataset"):
    val DiscountId   = col("c_discount_id",      int8)
    val DatasetId    = col("c_dataset_id",       dataset_id)

  object TimeChargeDiscountOverlapTable extends TableDef("t_time_charge_discount_overlap"):
    val DiscountId    = col("c_discount_id",      int8)
    val ObservationId = col("c_observation_id",   observation_id)