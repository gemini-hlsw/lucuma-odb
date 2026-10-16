// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.data.NonEmptyChain
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.StepViolation
import munit.FunSuite

class StepViolationValidatorSuite extends FunSuite:

  private val error   = StepViolation(ObservationValidationCode.ConfigurationError,  "Exposure times for GMOS North must be at least 1 s.")
  private val warning = StepViolation(ObservationValidationCode.ExposureTimeWarning, "Exposure times above 1200 s are not recommended for GMOS North.")

  test("a step violation is reported under its own code"):
    assertEquals(StepViolationValidator.validation(SequenceType.Science, error).code, ObservationValidationCode.ConfigurationError)
    assertEquals(StepViolationValidator.validation(SequenceType.Science, warning).code, ObservationValidationCode.ExposureTimeWarning)

  test("messages name the sequence"):
    assertEquals(
      StepViolationValidator.validation(SequenceType.Acquisition, error).messages,
      NonEmptyChain.one("Acquisition sequence: Exposure times for GMOS North must be at least 1 s.")
    )
    assertEquals(
      StepViolationValidator.message(SequenceType.Science, warning),
      "Science sequence: Exposure times above 1200 s are not recommended for GMOS North."
    )
