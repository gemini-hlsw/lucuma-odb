// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow.validator

import cats.syntax.all.*
import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.Observatory
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.service.workflow.ObservationValidationInfo
import lucuma.odb.service.workflow.ObservationValidator

// Exchange observations must match the proposal's observatory, and (when
// the call restricts instruments) use one of its allowed exchange instruments.
object ExchangeValidator extends ObservationValidator:

  def exchangeObservatoryMismatch(modeObs: Observatory, cfpObs: Observatory): String =
    s"Exchange observation requires a $modeObs Call for Proposals, but the proposal's observatory is $cfpObs."

  def invalidExchangeInstrument(instr: String): String =
    s"Instrument $instr is not part of the Call for Proposals."

  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    info.observingMode match
      case Some(e: ExchangeObservingModeType) =>
        info.cfpInfo.foldMap: cfp =>
          if cfp.observatory =!= e.observatory then
            ObservationValidationMap.singleton(ObservationValidation.callForProposals(exchangeObservatoryMismatch(e.observatory, cfp.observatory)))
          else e match
            case ExchangeObservingModeType.ExchangeKeck =>
              if cfp.keckInstruments.isEmpty then ObservationValidationMap.empty
              else info.keckInstrument.foldMap: inst =>
                if cfp.keckInstruments.contains(inst) then ObservationValidationMap.empty
                else ObservationValidationMap.singleton(ObservationValidation.callForProposals(invalidExchangeInstrument(inst.tag)))
            case ExchangeObservingModeType.ExchangeSubaru =>
              if cfp.subaruInstruments.isEmpty then ObservationValidationMap.empty
              else info.subaruInstrument.foldMap: inst =>
                if cfp.subaruInstruments.contains(inst) then ObservationValidationMap.empty
                else ObservationValidationMap.singleton(ObservationValidation.callForProposals(invalidExchangeInstrument(inst.tag)))
      case _ => ObservationValidationMap.empty