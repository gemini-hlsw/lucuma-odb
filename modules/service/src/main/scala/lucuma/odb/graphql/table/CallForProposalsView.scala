// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import skunk.codec.boolean.bool
import skunk.codec.temporal.date

trait CallForProposalsView[F[_]] extends BaseMapping[F]:

  object CallForProposalsView extends TableDef("v_cfp"):
    val Id              = col("c_cfp_id",           cfp_id)
    val Title           = col("c_title",            text_nonempty)
    val Semester        = col("c_semester",         semester)
    val DeadlineDefault = col("c_deadline_default", core_timestamp.opt)
    val ActiveStart     = col("c_active_start",     date)
    val ActiveEnd       = col("c_active_end",       date)
    val Existence       = col("c_existence",        existence)
    val IsOpen          = col("c_is_open",          bool)
    val Observatory     = col("c_observatory",      observatory)


    object coordinateLimits:
      object north:
        val RaStart  = col("c_north_ra_start",     right_ascension)
        val RaEnd    = col("c_north_ra_end",       right_ascension)
        val DecStart = col("c_north_dec_start",    declination)
        val DecEnd   = col("c_north_dec_end",      declination)

      object south:
        val RaStart  = col("c_south_ra_start",     right_ascension)
        val RaEnd    = col("c_south_ra_end",       right_ascension)
        val DecStart = col("c_south_dec_start",    declination)
        val DecEnd   = col("c_south_dec_end",      declination)

    object gemini:
      val Id                 = col("c_gemini_cfp_id",        cfp_id.embedded)
      val Type               = col("c_type",                 cfp_type)
      val Instruments        = col("c_instruments",          _instrument)
      val Proprietary        = col("c_proprietary",          int4_nonneg)
      val AllowsNonPartner   = col("c_allows_non_partner",   bool)
      val NonPartnerDeadline = col("c_non_partner_deadline", core_timestamp.opt)
      val ExchangePartners   = col("c_exchange_partners",    _exchange_partner)

    object keck:
      val Id                 = col("c_keck_cfp_id",          cfp_id.embedded)
      val Instruments        = col("c_keck_instruments",     _keck_instrument)

    object subaru:
      val Id                 = col("c_subaru_cfp_id",         cfp_id.embedded)
      val Type               = col("c_subaru_proposal_type",  subaru_proposal_type)
      val Instruments        = col("c_subaru_instruments",    _subaru_instrument)

  object CallForProposalsPartnerView extends TableDef("v_cfp_partner"):
    val CfpId            = col("c_cfp_id",            cfp_id)
    val Partner          = col("c_partner",           partner)
    val DeadlineOverride = col("c_deadline_override", core_timestamp.opt)
    val Deadline         = col("c_deadline",          core_timestamp.opt)

  object CallForProposalsInstrumentTable extends TableDef("t_cfp_instrument"):
    val CfpId      = col("c_cfp_id",     cfp_id)
    val Instrument = col("c_instrument", instrument)