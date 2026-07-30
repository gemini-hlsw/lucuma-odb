// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap

import ObservationWorkflowService.*

val cfpInstrumentValidator: Validator = info =>
  info.cfpInfo.foldMap: cfp =>
    if cfp.instruments.isEmpty then ObservationValidationMap.empty // weird but original logic does this
    else info.instrument.foldMap: inst =>
      if cfp.instruments.contains(inst) then ObservationValidationMap.empty
      else ObservationValidationMap.singleton(ObservationValidation.callForProposals(Messages.invalidInstrument(inst)))
