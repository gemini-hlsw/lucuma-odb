// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.Observatory
import lucuma.core.enums.Partner
import lucuma.core.enums.ScienceSubtype
import lucuma.core.util.Enumerated

/**
 * The layouts a proposal summary PDF can be rendered in.
 */
enum SummaryStyle(val tag: String, val rendererName: String) derives Enumerated:
  case GeminiStandard           extends SummaryStyle("gemini_standard",             "gemini-standard")
  case GeminiDarp               extends SummaryStyle("gemini_darp",                 "gemini-darp")
  case GeminiNoInvestigators    extends SummaryStyle("gemini_no_investigators",     "gemini-no-investigators")
  case GeminiInvestigatorsAtEnd extends SummaryStyle("gemini_investigators_at_end", "gemini-investigators-at-end")
  case Chile                    extends SummaryStyle("chile",                       "chile")
  case NoirlabDarp              extends SummaryStyle("noirlab_darp",                "noirlab-darp")

object SummaryStyle:

  val Default: SummaryStyle = GeminiStandard

  /**
   * What the legacy OCS renders today: P1MonitorConfig.toTemplate over
   * conf.production-2026B.xml.  Fast Turnaround departs deliberately, sc-10424
   * renders it without investigators where OCS uses darp.
   *
   * The proposal type decides on its own; only queue and classical proposals,
   * which are the ones apportioned across partners, follow the partner.  An
   * exchange partner takes the whole time request, so it stands in for the
   * splits when there is one.
   */
  def forProposal(
    subtype:         Option[ScienceSubtype],
    observatory:     Observatory,
    exchangePartner: Option[ExchangePartner],
    partner:         Option[Partner]
  ): SummaryStyle =
    observatory match
      // Normal and intensive Subaru proposals alike.
      case Observatory.Subaru => GeminiDarp
      // Keck is not in use; Default until someone picks a template for it.
      case Observatory.Keck   => Default
      case Observatory.Gemini => subtype.fold(Default):
        case ScienceSubtype.LargeProgram                                     => GeminiDarp
        case ScienceSubtype.DemoScience | ScienceSubtype.SystemVerification  => GeminiInvestigatorsAtEnd
        case ScienceSubtype.FastTurnaround                                   => GeminiNoInvestigators
        case ScienceSubtype.DirectorsTime | ScienceSubtype.PoorWeather       => GeminiStandard
        case ScienceSubtype.Classical | ScienceSubtype.Queue                 =>
          exchangePartner.fold(forPartner(partner)):
            case ExchangePartner.Subaru => GeminiDarp
            case ExchangePartner.Keck   => Default

  private def forPartner(partner: Option[Partner]): SummaryStyle =
    partner.fold(Default):
      case Partner.CL => Chile
      case Partner.UH => GeminiStandard
      case Partner.US => NoirlabDarp
      case Partner.AR |
           Partner.BR |
           Partner.CA |
           Partner.KR => GeminiDarp
