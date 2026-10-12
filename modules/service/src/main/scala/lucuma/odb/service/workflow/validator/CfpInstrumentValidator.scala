// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.Observatory
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap

// A Gemini call with no instruments listed places no restriction on them.  An
// exchange (Keck/Subaru) call offers no Gemini instruments at all, so any Gemini
// instrument in a proposal for one is invalid.  Exchange observations have no
// Gemini instrument and are checked by ExchangeValidator instead.
object CfpInstrumentValidator extends ObservationValidator:

  def invalidInstrument(instr: Instrument): String =
    s"Instrument $instr not part of Call for Proposals."

  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    info.cfpInfo.foldMap: cfp =>
      if cfp.observatory === Observatory.Gemini && cfp.instruments.isEmpty then ObservationValidationMap.empty
      else info.instrument.foldMap: inst =>
        if cfp.instruments.contains(inst) then ObservationValidationMap.empty
        else ObservationValidationMap.singleton(ObservationValidation.callForProposalsError(invalidInstrument(inst)))

