// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ItcInputDerivation
import lucuma.odb.sequence.data.MissingParamSet
import lucuma.odb.service.GeneratorParamsService
import lucuma.odb.service.GeneratorParamsService.Error as GenParamsError
import lucuma.odb.service.workflow.ObservationValidator

extension (mp: MissingParamSet)
  def toObsValidation: ObservationValidation =
    ObservationValidation.configuration(s"Missing ${mp.params.map(_.name).toList.intercalate(", ")}")

extension (ge: GeneratorParamsService.Error)
  def toObsValidation: ObservationValidation = ge match
    case GenParamsError.MissingData(p) => p.toObsValidation
    case _                             => ObservationValidation.configuration(ge.format)

val GeneratorValidator: ObservationValidator = info =>
  if info.isVisitor || info.isExchange then ObservationValidationMap.empty
  else info.generatorParams.foldMap:
    case Left(error)                                                         => ObservationValidationMap.singleton(error.toObsValidation)
    case Right(GeneratorParams(itcInput = ItcInputDerivation.Incomplete(m))) => ObservationValidationMap.singleton(m.toObsValidation)
    case Right(ps)                                                           => ObservationValidationMap.empty

