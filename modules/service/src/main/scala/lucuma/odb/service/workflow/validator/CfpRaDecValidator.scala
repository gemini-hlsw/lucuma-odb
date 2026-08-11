// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap

object CfpRaDecValidator extends ObservationValidator:

  val CoordinatesOutOfRange = "Base coordinates out of Call for Proposals limits."

  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    info.cfpInfo.foldMap: cfp =>
      info.site.foldMap: site =>
        info.coordinates.foldMap: coords =>
          val ok = cfp.limits.siteLimits(site).inLimits(coords)
          if ok then ObservationValidationMap.empty
          else ObservationValidationMap.singleton(ObservationValidation.callForProposals(CoordinatesOutOfRange))
