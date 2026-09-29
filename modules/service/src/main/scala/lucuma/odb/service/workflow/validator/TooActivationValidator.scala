// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import lucuma.core.enums.TooActivation
import lucuma.core.model.ObservationValidation
import lucuma.core.syntax.string.*
import lucuma.odb.data.ObservationValidationMap

// The Target-of-Opportunity activation, checked against the program's ceiling.
// Exceeding it is an authorization failure rather than a misconfiguration, so it
// maps to Unapproved -- the observation cannot advance to Ready until the
// activation is lowered or staff raise the ceiling.  The activation is approved
// program-wide rather than per configuration, so it is checked here rather than
// with the configuration requests.
object TooActivationValidator extends ObservationValidator:

  def tooActivationUnexpected(obs: TooActivation): String =
    s"Target of Opportunity activation ${obs.tag.toScreamingSnakeCase} is not ordinarily available to this " +
    "proposal type: acceptance will limit it to NONE unless staff grant more."

  def tooActivationUnapproved(obs: TooActivation, ceiling: TooActivation): String =
    s"Target of Opportunity activation ${obs.tag.toScreamingSnakeCase} has not been approved: the program " +
    s"allows at most ${ceiling.tag.toScreamingSnakeCase}."

  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    info.tooCeiling match
      case Some(ceiling) if info.exceedsTooCeiling =>
        ObservationValidationMap.singleton:
          ObservationValidation.tooActivationUnapproved(tooActivationUnapproved(info.tooActivation, ceiling))
      case _ if info.tooUnexpected                  =>
        ObservationValidationMap.singleton:
          ObservationValidation.Warning.tooActivationUnexpected(tooActivationUnexpected(info.tooActivation))
      case _                                        =>
        ObservationValidationMap.empty
