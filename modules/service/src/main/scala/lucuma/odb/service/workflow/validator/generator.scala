// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ItcInputDerivation

import ObservationWorkflowService.*

val generatorValidator: Validator = info =>
  if info.isVisitor || info.isExchange then ObservationValidationMap.empty
  else info.generatorParams.foldMap:
    case Left(error)                                                                   => ObservationValidationMap.singleton(error.toObsValidation)
    case Right(GeneratorParams(ItcInputDerivation.Incomplete(m), _, _, _, _, _, _, _)) => ObservationValidationMap.singleton(m.toObsValidation)
    case Right(ps)                                                                     => ObservationValidationMap.empty
