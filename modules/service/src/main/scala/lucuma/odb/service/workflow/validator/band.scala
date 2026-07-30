// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap

import ObservationWorkflowService.*

val bandValidator: Validator = info =>
  (info.scienceBand, info.programAllocations).tupled.foldMap: (b, bs) =>
    if bs.toList.contains(b) then ObservationValidationMap.empty
    else ObservationValidationMap.singleton(ObservationValidation.configuration(Messages.invalidScienceBand(b)))