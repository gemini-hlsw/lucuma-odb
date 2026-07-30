// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.data.NonEmptyChain
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap

val otherConfigErrorValidator: Validator = info =>
  NonEmptyChain.fromSeq(info.otherConfigErrors) match
    case None       => ObservationValidationMap.empty
    case Some(errs) => ObservationValidationMap.singleton(ObservationValidation(ObservationValidationCode.ConfigurationError, errs))
