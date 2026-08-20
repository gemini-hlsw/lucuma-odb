// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.core.math.Wavelength
import lucuma.core.model.IntCentiPercent
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap
import lucuma.core.math.Declination
import lucuma.core.enums.Site

object ConditionsProbabilityValidator extends ObservationValidator:

  val limit = IntCentiPercent.unsafeFromPercent(10)

  def apply(info: ObservationValidationInfo): ObservationValidationMap = 
    // TODO: what do we do for imaging?
    (info.spectroscopyWavelength, info.coordinates.map(_.dec), info.site)
      .tupled
      .map(info.constraintSet.likelihood)
      .filter(p => p.value.value < limit.value.value)
      .foldMap: percent =>
        val w = ObservationValidation.genericWaning(s"Conditions likelihood is ${percent.toPercent.toInt}%.")
        ObservationValidationMap.singleton(w)

