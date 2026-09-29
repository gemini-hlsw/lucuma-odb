// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import lucuma.core.enums.ObservationWorkflowState.Ready
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap

object OpportunityTargetValidator extends ObservationValidator:

  val OpportunityTargetNotSwapped =
    "Replace the Target of Opportunity placeholder with the actual target before setting the observation Ready."

  // An opportunity target is a placeholder: it carries a region and nothing to
  // slew to.  Holding one is perfectly valid -- it is how a ToO waits, and how
  // the proposal described it -- so this fires only against Ready, which is the
  // trigger.  Asking an observer to act on an observation with no coordinates is
  // the one thing that must not happen.
  //
  // This is a backstop rather than the primary gate.  The Defined -> Ready
  // transition is refused while a placeholder is in the asterism, but Ready is a
  // pre-execution state, so the asterism can still be edited afterwards -- and a
  // Ready ToO with nowhere to point is worse than a loud error.
  //
  // Note there is no longer any check relating the asterism to the activation.
  // Being a Target of Opportunity is declared on the observation, so an
  // opportunity target neither makes an observation a ToO nor is required by one;
  // a ToO whose target was known from the outset never holds a placeholder at all.
  // The one cross-axis rule -- that Rapid and Interrupting require
  // Uninterruptible -- is rejected at the mutation rather than surfaced here.
  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    if info.hasTooTarget && info.effectiveUserState.contains(Ready) then
      ObservationValidationMap.singleton(ObservationValidation.configuration(OpportunityTargetNotSwapped))
    else ObservationValidationMap.empty
