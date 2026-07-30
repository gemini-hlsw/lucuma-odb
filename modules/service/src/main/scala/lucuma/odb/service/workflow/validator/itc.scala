// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.Itc
import lucuma.odb.data.ObservationValidationMap

def itcValidator(itcFor: Observation.Id => Option[Itc]): Validator = info =>
  if itcFor(info.oid).isDefined || info.isVisitor || info.isExchange then ObservationValidationMap.empty
  else ObservationValidationMap.singleton(ObservationValidation.itc("ITC results are not present."))