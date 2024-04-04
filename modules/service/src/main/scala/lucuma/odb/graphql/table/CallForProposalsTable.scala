// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.cfp_id
import lucuma.odb.util.Codecs.cfp_status
import lucuma.odb.util.Codecs.cfp_type
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.declination
import lucuma.odb.util.Codecs.existence
import lucuma.odb.util.Codecs.instrument
import lucuma.odb.util.Codecs.partner
import lucuma.odb.util.Codecs.right_ascension
import lucuma.odb.util.Codecs.semester
import lucuma.odb.util.Codecs.timestamp_interval_tsrange


trait CallForProposalsTable[F[_]] extends BaseMapping[F] {

  object CallForProposalsTable extends TableDef("t_cfp") {
    val Id       = col("c_cfp_id",     cfp_id)
    val Status   = col("c_status",     cfp_status)
    val Type     = col("c_type",       cfp_type)
    val Semester = col("c_semester",   semester)

    val RaStart  = col("c_ra_start",   right_ascension.embedded)
    val RaEnd    = col("c_ra_end",     right_ascension.embedded)

    val DecStart = col("c_dec_start",  declination.embedded)
    val DecEnd   = col("c_dec_end",    declination.embedded)

    val Active   = col("c_active",     timestamp_interval_tsrange)

    val Existence = col("c_existence", existence)
  }

  object CallForProposalsPartnerTable extends TableDef("t_cfp_partner") {
    val CfpId    = col("c_cfp_id",   cfp_id)
    val Partner  = col("c_partner",  partner)
    val Deadline = col("c_deadline", core_timestamp)
  }

  object CallForProposalsInstrumentTable extends TableDef("t_cfp_instrument") {
    val CfpId      = col("c_cfp_id",     cfp_id)
    val Instrument = col("c_instrument", instrument)
  }

}
