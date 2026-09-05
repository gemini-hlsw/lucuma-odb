// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.core.enums.ObservationWorkflowState.Ready
import lucuma.core.enums.SchedulingMode
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap

object OpportunityTargetValidator extends ObservationValidator:

  val OpportunityTargetNotResolved =
    "Replace the Target of Opportunity placeholder with the actual target coordinates."

  val InterruptingRequiresOpportunityTarget =
    "An observation may only interrupt executing science if it is a Target of Opportunity; add an opportunity target or lower the scheduling mode."

  // Two ways an observation can be internally inconsistent about being a Target
  // of Opportunity.  Both are ConfigurationErrors -- Undefined, which also
  // suppresses a stored Ready.
  //
  // The first is a backstop.  An unresolved target already blocks the
  // Defined -> Ready transition, so a trigger cannot be requested for one, but
  // the asterism can still be edited afterwards (Ready is in preExecutionSet)
  // and a Ready ToO with no coordinates is worse than a loud error.  Note the
  // target is no longer replaced when the alert arrives; it keeps its region and
  // gains a resolution, so "has an opportunity target" and "is still waiting"
  // are two different questions.
  //
  // The second is the one dependency this design validates rather than making
  // structural.  Interrupting is the only mode reserved to ToOs -- science staff
  // report no application for an interrupting observation that is not one -- and
  // rejecting the combination is what keeps "is this a ToO" the single test
  // `hasTooTarget`, with no disjunction in it.
  //
  // There is no longer any check that an opportunity target implies an
  // activation: the activation is now derived from the target, so the two cannot
  // disagree.
  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    if info.hasTooTarget && info.hasUnresolvedTooTarget && info.effectiveUserState.contains(Ready) then
      ObservationValidationMap.singleton(ObservationValidation.configuration(OpportunityTargetNotResolved))
    else if info.schedulingMode === SchedulingMode.Interrupting && !info.hasTooTarget then
      ObservationValidationMap.singleton(ObservationValidation.configuration(InterruptingRequiresOpportunityTarget))
    else ObservationValidationMap.empty
