// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepViolation
import lucuma.core.model.sequence.exposure.ExposureRules
import lucuma.core.model.sequence.exposure.PendingExposureRules
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.odb.sequence.gmos.SecondOrderOverlap

/**
 * The problems found in a step of a sequence, as validations.  Steps are
 * checked as the sequence digest is computed, so generated and edited
 * sequences alike are covered.  Every instrument's exposure times are checked
 * against its exposure rules (see `ExposureRules.check`); an
 * instrument may add checks of its own, such as GMOS's second order overlap.
 * A new kind of step check is an addition here and a validation code for it.
 */
trait StepCheck[D]:
  def violations(step: Step[D], ctx: PendingExposureRules.Context): List[StepViolation]

object StepCheck:

  def apply[D](using ev: StepCheck[D]): StepCheck[D] =
    ev

  private def exposure[D: PendingExposureRules](step: Step[D], ctx: PendingExposureRules.Context): List[StepViolation] =
    ExposureRules.check(step, ctx)

  private def exposureOnly[D: PendingExposureRules]: StepCheck[D] =
    exposure(_, _)

  given StepCheck[Flamingos2DynamicConfig] = exposureOnly
  given StepCheck[GhostDynamicConfig]      = exposureOnly
  given StepCheck[GnirsDynamicConfig]      = exposureOnly
  given StepCheck[Igrins2DynamicConfig]    = exposureOnly

  given StepCheck[DynamicConfig.GmosNorth] = (step, ctx) =>
    exposure(step, ctx) ++ SecondOrderOverlap.gmosNorth(step)

  given StepCheck[DynamicConfig.GmosSouth] = (step, ctx) =>
    exposure(step, ctx) ++ SecondOrderOverlap.gmosSouth(step)
