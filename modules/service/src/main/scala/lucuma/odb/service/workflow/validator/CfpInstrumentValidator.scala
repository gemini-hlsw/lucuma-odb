// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap

object CfpInstrumentValidator extends ObservationValidator:

  def invalidInstrument(instr: Instrument): String =
    s"Instrument $instr not part of Call for Proposals."

  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    info.cfpInfo.foldMap: cfp =>
      if cfp.instruments.isEmpty then ObservationValidationMap.empty // weird but original logic does this
      else info.instrument.foldMap: inst =>
        if cfp.instruments.contains(inst) then ObservationValidationMap.empty
        else ObservationValidationMap.singleton(ObservationValidation.callForProposals(invalidInstrument(inst)))

