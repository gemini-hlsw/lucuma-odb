// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.charge_class
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.text_nonempty
import lucuma.odb.util.Codecs.time_charge_correction_op
import lucuma.odb.util.Codecs.time_span
import lucuma.odb.util.Codecs.user_id
import lucuma.odb.util.Codecs.visit_id
import skunk.codec.numeric.int8

trait TimeChargeCorrectionTable[F[_]] extends BaseMapping[F] {

  object TimeChargeCorrectionTable extends TableDef("t_time_charge_correction") {

    val Id          = col("c_id",           int8)
    val VisitId     = col("c_visit_id",     visit_id)
    val Created     = col("c_created",      core_timestamp)
    val ChargeClass = col("c_charge_class", charge_class)
    val Op          = col("c_op",           time_charge_correction_op)
    val Amount      = col("c_amount",       time_span)
    val UserId      = col("c_user_id",      user_id)
    val Comment     = col("c_comment",      text_nonempty.opt)

  }

}
