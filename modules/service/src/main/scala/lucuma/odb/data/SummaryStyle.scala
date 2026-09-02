// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.enums.Partner
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

  // The OCS Phase 1 template map (P1PDF.templatesList).  A total match so
  // that a new Partner forces a decision here.
  def forPartner(partner: Option[Partner]): SummaryStyle =
    partner.fold(Default):
      case Partner.CA => GeminiInvestigatorsAtEnd
      case Partner.CL => Chile
      case Partner.KR => GeminiDarp
      case Partner.US => NoirlabDarp
      case Partner.AR |
           Partner.BR |
           Partner.UH => Default
