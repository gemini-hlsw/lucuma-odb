// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import lucuma.core.enums.ObservationWorkflowState.Ready
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap

object OpportunityTargetValidator extends ObservationValidator:
  
  val OpportunityTargetRequiresActivation =
    "An observation with a Target of Opportunity placeholder must set a ToO activation other than NONE."

  val OpportunityTargetNotResolved =
    "Replace the Target of Opportunity placeholder with the actual target coordinates."

  // An opportunity target is a placeholder standing in for a target that
  // has not been found yet, so it makes sense only in an observation that
  // declares itself a Target of Opportunity, and only until the alert
  // arrives.  Either way the observation is internally inconsistent rather
  // than merely unapproved, so this is a ConfigurationError -- Undefined,
  // which also suppresses a stored Ready.
  //
  // The second case is a backstop.  A placeholder already blocks the
  // Defined -> Ready transition, so a trigger cannot be requested for one,
  // but the asterism can still be edited afterwards (Ready is in
  // preExecutionSet) and a Ready ToO with no coordinates is worse than a
  // loud error.
  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    if !info.hasTooTarget then ObservationValidationMap.empty
    else if !info.isTooObservation then
      ObservationValidationMap.singleton(ObservationValidation.configuration(OpportunityTargetRequiresActivation))
    else if info.effectiveUserState.contains(Ready) then
      ObservationValidationMap.singleton(ObservationValidation.configuration(OpportunityTargetNotResolved))
    else ObservationValidationMap.empty