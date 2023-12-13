// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.time_span
import lucuma.odb.util.Codecs.visit_id

trait TimeAccountingTable[F[_]] extends BaseMapping[F] {

  object TimeAccountingTable extends TableDef("t_time_accounting") {

    val VisitId          = col("c_visit_id",             visit_id)

    object Raw {
      val NonChargedTime = col("c_raw_non_charged_time", time_span)
      val PartnerTime    = col("c_raw_partner_time",     time_span)
      val ProgramTime    = col("c_raw_program_time",     time_span)
    }

    object Final {
      val NonChargedTime = col("c_final_non_charged_time", time_span)
      val PartnerTime    = col("c_final_partner_time",     time_span)
      val ProgramTime    = col("c_final_program_time",     time_span)
    }

  }

}
