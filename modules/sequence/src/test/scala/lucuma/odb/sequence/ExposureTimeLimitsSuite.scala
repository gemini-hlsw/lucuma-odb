// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.syntax.*
import lucuma.core.enums.SequenceType
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.ExposureTimeIssue.Kind

class ExposureTimeLimitsSuite extends munit.FunSuite:

  val all: ExposureTimeLimits =
    ExposureTimeLimits(1.secTimeSpan.some, 2.secTimeSpan.some, 10.secTimeSpan.some, 20.secTimeSpan.some)

  test("within the recommended range is fine"):
    assertEquals(all.classify(2.secTimeSpan), none)
    assertEquals(all.classify(5.secTimeSpan), none)
    assertEquals(all.classify(10.secTimeSpan), none)

  test("outside the recommended range is a warning"):
    assertEquals(all.classify(1.secTimeSpan), (Kind.BelowRecommendedMinimum, 2.secTimeSpan).some)
    assertEquals(all.classify(20.secTimeSpan), (Kind.AboveRecommendedMaximum, 10.secTimeSpan).some)

  test("outside the limits is an error, taking precedence over the warning"):
    assertEquals(all.classify(500.msTimeSpan), (Kind.BelowMinimum, 1.secTimeSpan).some)
    assertEquals(all.classify(21.secTimeSpan), (Kind.AboveMaximum, 20.secTimeSpan).some)

  test("a missing limit is not checked"):
    assertEquals(ExposureTimeLimits.Unlimited.classify(TimeSpan.Zero), none)
    assertEquals(ExposureTimeLimits.minimum(1.secTimeSpan).classify(1000.secTimeSpan), none)

  test("json round trip"):
    val i = ExposureTimeIssue(SequenceType.Acquisition, Kind.BelowMinimum, "GMOS North", 1.secTimeSpan, PosInt.unsafeFrom(3), 500.msTimeSpan)
    assertEquals(i.asJson.as[ExposureTimeIssue], i.asRight)
