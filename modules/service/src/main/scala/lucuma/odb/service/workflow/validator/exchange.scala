// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap

import ObservationWorkflowService.*

// Exchange observations must match the proposal's observatory, and (when
// the call restricts instruments) use one of its allowed exchange instruments.
val exchangeValidator: Validator = info =>
  info.observingMode match
    case Some(e: ExchangeObservingModeType) =>
      info.cfpInfo.foldMap: cfp =>
        if cfp.observatory =!= e.observatory then
          ObservationValidationMap.singleton(ObservationValidation.callForProposals(Messages.exchangeObservatoryMismatch(e.observatory, cfp.observatory)))
        else e match
          case ExchangeObservingModeType.ExchangeKeck =>
            if cfp.keckInstruments.isEmpty then ObservationValidationMap.empty
            else info.keckInstrument.foldMap: inst =>
              if cfp.keckInstruments.contains(inst) then ObservationValidationMap.empty
              else ObservationValidationMap.singleton(ObservationValidation.callForProposals(Messages.invalidExchangeInstrument(inst.tag)))
          case ExchangeObservingModeType.ExchangeSubaru =>
            if cfp.subaruInstruments.isEmpty then ObservationValidationMap.empty
            else info.subaruInstrument.foldMap: inst =>
              if cfp.subaruInstruments.contains(inst) then ObservationValidationMap.empty
              else ObservationValidationMap.singleton(ObservationValidation.callForProposals(Messages.invalidExchangeInstrument(inst.tag)))
    case _ => ObservationValidationMap.empty
