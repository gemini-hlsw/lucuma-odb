// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.exposure.ExposureTimeViolation
import munit.FunSuite

class ExposureTimeValidatorSuite extends FunSuite:

  private val error   = ExposureTimeViolation(ExposureTimeViolation.Severity.Error,   "Exposure times for GMOS North must be at least 1 s.")
  private val warning = ExposureTimeViolation(ExposureTimeViolation.Severity.Warning, "Exposure times above 1200 s are not recommended for GMOS North.")

  test("exposures that cannot be taken are configuration errors"):
    assertEquals(ExposureTimeValidator.validation(SequenceType.Science, error).code, ObservationValidationCode.ConfigurationError)

  test("exposures outside the recommended range are exposure time warnings"):
    assertEquals(ExposureTimeValidator.validation(SequenceType.Science, warning).code, ObservationValidationCode.ExposureTimeWarning)

  test("messages name the sequence"):
    assertEquals(
      ExposureTimeValidator.message(SequenceType.Acquisition, error),
      "Acquisition sequence: Exposure times for GMOS North must be at least 1 s."
    )
    assertEquals(
      ExposureTimeValidator.message(SequenceType.Science, warning),
      "Science sequence: Exposure times above 1200 s are not recommended for GMOS North."
    )
