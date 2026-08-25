// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.boolean.bool

trait ProposalView[F[_]] extends BaseMapping[F]:

  object ProposalView extends TableDef("v_proposal"):

    val ProgramId       = col("c_program_id",        program_id)
    val GeminiId        = col("c_program_id_gemini", program_id.embedded)
    // Non-null discriminator for the GeminiProposalType interface.  External
    // (exchange) proposals have no science subtype, so v_proposal coalesces it to
    // a placeholder; the GeminiProposalType object's key (GeminiId) is null for
    // those rows, so the placeholder is never rendered or discriminated upon.
    val ScienceSubtype  = col("c_gemini_science_subtype", science_subtype)

    val Category        = col("c_category", tag.opt)

    // Explicit / default / effective ToO ceiling.  The stored column is the
    // explicit choice (null when nobody has made one); the other two are
    // computed in the view.  See the ceiling discussion in V1241.
    val TooActivationCeilingExplicit  = col("c_too_activation", too_activation.opt)
    val TooActivationCeilingDefault   = col("c_too_activation_default", too_activation)
    val TooActivationCeilingEffective = col("c_too_activation_effective", too_activation)
    val MinPercent      = col("c_min_percent",    int_percent)
    val ExchangePartner = col("c_exchange_partner", exchange_partner.opt)

    // The explicitly requested observing time, null when nobody has chosen one
    // and the sum over the program's observations stands instead.  The default
    // and effective values are not view columns: the derivation is the group
    // tree fold in TimeEstimateService, so they come from an effect handler.
    val TimeRequestExplicit   = col("c_time_request", time_span.embedded)
    val TimeRequestExplicitId = col("c_time_request_id", program_id.embedded)

    val CallId          = col("c_cfp_id", cfp_id.opt)

    object Classical:
      val Id                      = col("c_program_id_c", program_id.embedded)
      val AeonMultiFacilityId     = col("c_aeon_multi_facility_id", program_id.embedded)
      val AeonRequiredInstruments = col("c_aeon_required_instruments", _instrument)
      val JwstSynergy             = col("c_jwst_synergy", bool)
      val UsLongTerm              = col("c_us_long_term", bool)

    object DemoScience:
      val Id = col("c_program_id_s", program_id.embedded)

    object DirectorsTime:
      val Id = col("c_program_id_d", program_id.embedded)

    object FastTurnaround:
      val Id         = col("c_program_id_f", program_id.embedded)
      val ReviewerId = col("c_reviewer_id", program_user_id.opt)
      val MentorId   = col("c_mentor_id", program_user_id.opt)

    object LargeProgram:
      val Id                      = col("c_program_id_l", program_id.embedded)
      val MinPercentTotal         = col("c_min_percent_total", int_percent)
      val TotalTime               = col("c_total_time", time_span)
      val AeonMultiFacilityId     = col("c_aeon_multi_facility_id", program_id.embedded)
      val AeonRequiredInstruments = col("c_aeon_required_instruments", _instrument)
      val JwstSynergy             = col("c_jwst_synergy", bool)

    object PoorWeather:
      val Id  = col("c_program_id_p", program_id.embedded)

    object Queue:
      val Id                      = col("c_program_id_q", program_id.embedded)
      val AeonMultiFacilityId     = col("c_aeon_multi_facility_id", program_id.embedded)
      val AeonRequiredInstruments = col("c_aeon_required_instruments", _instrument)
      val JwstSynergy             = col("c_jwst_synergy", bool)
      val UsLongTerm              = col("c_us_long_term", bool)
      val ConsiderForBand3        = col("c_consider_for_band_3", consider_for_band_3)

    object SystemVerification:
      val Id = col("c_program_id_v", program_id.embedded)

    object Keck:
      val Id = col("c_program_id_keck", program_id.embedded)

    object Subaru:
      val Id = col("c_program_id_subaru", program_id.embedded)
