// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs._instrument
import lucuma.odb.util.Codecs.cfp_id
import lucuma.odb.util.Codecs.cfp_type
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.declination
import lucuma.odb.util.Codecs.existence
import lucuma.odb.util.Codecs.instrument
import lucuma.odb.util.Codecs.right_ascension
import lucuma.odb.util.Codecs.semester
import lucuma.odb.util.Codecs.tag
import lucuma.odb.util.Codecs.timestamp_interval_tsrange


trait CallForProposalsView[F[_]] extends BaseMapping[F] {

  object CallForProposalsView extends TableDef("v_cfp") {
    val Id       = col("c_cfp_id",     cfp_id)
    val Type     = col("c_type",       cfp_type)
    val Semester = col("c_semester",   semester)

    val RaStartId  = col("c_ra_start_id",  cfp_id.embedded)
    val RaStart    = col("c_ra_start",     right_ascension.embedded)
    val RaEndId    = col("c_ra_end_id",    cfp_id.embedded)
    val RaEnd      = col("c_ra_end",       right_ascension.embedded)
    val DecStartId = col("c_dec_start_id", cfp_id.embedded)
    val DecStart   = col("c_dec_start",    declination.embedded)
    val DecEndId   = col("c_dec_end_id",   cfp_id.embedded)
    val DecEnd     = col("c_dec_end",      declination.embedded)

    val Active   = col("c_active",     timestamp_interval_tsrange)

    val Existence = col("c_existence", existence)

    val Instruments = col("c_instruments", _instrument)
  }

  object CallForProposalsPartnerTable extends TableDef("t_cfp_partner") {
    val CfpId    = col("c_cfp_id",   cfp_id)
    val Partner  = col("c_partner",  tag)
    val Deadline = col("c_deadline", core_timestamp)
  }

  object CallForProposalsInstrumentTable extends TableDef("t_cfp_instrument") {
    val CfpId      = col("c_cfp_id",     cfp_id)
    val Instrument = col("c_instrument", instrument)
  }

}
