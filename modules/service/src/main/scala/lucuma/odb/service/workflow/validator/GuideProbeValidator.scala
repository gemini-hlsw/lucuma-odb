// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service
package workflow
package validator

import cats.syntax.all.*
import lucuma.core.model.ObservationValidation
import lucuma.core.model.probes
import lucuma.odb.data.ObservationValidationMap

// Catches a mode change that leaves a previously valid explicit probe unusable.
object GuideProbeValidator extends ObservationValidator:

  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    (info.observingMode, info.explicitGuideProbe).tupled.foldMap: (mode, probe) =>
      if probes.isProbeAllowed(mode, probe) then ObservationValidationMap.empty
      else ObservationValidationMap.singleton(ObservationValidation.configuration(GuideProbeRules.notAllowedMessage(mode, probe)))
