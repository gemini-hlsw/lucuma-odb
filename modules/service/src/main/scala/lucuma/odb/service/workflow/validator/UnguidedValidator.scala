// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service
package workflow
package validator

import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap

object UnguidedValidator extends ObservationValidator:

  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    if info.explicitGuideProbe.exists(_.isUnguided) then
      ObservationValidationMap.singleton(ObservationValidation.Warning.unguided)
    else ObservationValidationMap.empty
