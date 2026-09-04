// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.Itc
import lucuma.odb.data.ObservationValidationMap

class ItcValidator(itcFor: Observation.Id => Option[Itc]) extends ObservationValidator:
  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    if itcFor(info.oid).isDefined || info.isVisitor || info.isExchange then ObservationValidationMap.empty
    else ObservationValidationMap.singleton(ObservationValidation.itc("ITC results are not present."))

object ItcValidator:
  def apply(itcFor: Observation.Id => Option[Itc]): ObservationValidator =
    new ItcValidator(itcFor)