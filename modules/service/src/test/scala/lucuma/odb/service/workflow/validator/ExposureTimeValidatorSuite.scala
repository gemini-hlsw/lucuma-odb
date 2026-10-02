// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.SequenceType
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.ExposureTimeIssue
import lucuma.odb.sequence.ExposureTimeIssue.Kind
import munit.FunSuite

class ExposureTimeValidatorSuite extends FunSuite:

  private def issue(kind: Kind, limit: TimeSpan, steps: Int, extreme: TimeSpan): ExposureTimeIssue =
    ExposureTimeIssue(SequenceType.Science, kind, "GMOS North", limit, PosInt.unsafeFrom(steps), extreme)

  test("exposures that cannot be taken are configuration errors"):
    assertEquals(ExposureTimeValidator.validation(issue(Kind.BelowMinimum, 1.secTimeSpan, 1, 500.msTimeSpan)).code, ObservationValidationCode.ConfigurationError)
    assertEquals(ExposureTimeValidator.validation(issue(Kind.AboveMaximum, 1.secTimeSpan, 1, 2.secTimeSpan)).code, ObservationValidationCode.ConfigurationError)

  test("exposures outside the recommended range are exposure time warnings"):
    assertEquals(ExposureTimeValidator.validation(issue(Kind.BelowRecommendedMinimum, 1.secTimeSpan, 1, 500.msTimeSpan)).code, ObservationValidationCode.ExposureTimeWarning)
    assertEquals(ExposureTimeValidator.validation(issue(Kind.AboveRecommendedMaximum, 1.secTimeSpan, 1, 2.secTimeSpan)).code, ObservationValidationCode.ExposureTimeWarning)

  test("messages"):
    assertEquals(
      ExposureTimeValidator.message(issue(Kind.BelowRecommendedMinimum, 2.secTimeSpan, 1, 1500.msTimeSpan)),
      "1 science step has an exposure time below the recommended 2 s minimum for GMOS North (shortest 1.5 s)."
    )
    assertEquals(
      ExposureTimeValidator.message(issue(Kind.AboveMaximum, 600.secTimeSpan, 3, 900.secTimeSpan)),
      "3 science steps have exposure times above the 600 s maximum for GMOS North (longest 900 s)."
    )
