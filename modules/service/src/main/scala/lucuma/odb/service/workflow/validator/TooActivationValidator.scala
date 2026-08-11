// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import lucuma.core.enums.TooActivation
import lucuma.core.model.ObservationValidation
import lucuma.core.syntax.string.*
import lucuma.odb.data.ObservationValidationMap

// The Target-of-Opportunity ceiling.  This is an authorization failure
// rather than a misconfiguration, so it maps to Unapproved -- the
// observation cannot advance to Ready until the activation is lowered or
// the proposal's ceiling is raised.
object TooActivationValidator extends ObservationValidator:

  def tooActivationExceedsCeiling(obs: TooActivation, ceiling: TooActivation): String =
    s"Target of Opportunity activation ${obs.tag.toScreamingSnakeCase} exceeds the maximum " +
    s"${ceiling.tag.toScreamingSnakeCase} allowed by the proposal."

  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    if !info.exceedsTooCeiling then ObservationValidationMap.empty
    else
      ObservationValidationMap.singleton:
        ObservationValidation.tooActivationUnapproved:
          tooActivationExceedsCeiling(info.tooActivation, info.tooCeiling.get)