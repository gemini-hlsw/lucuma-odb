// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.util.TimeSpan
import munit.FunSuite

class AcquisitionConfigSuite extends FunSuite:

  private def seconds(s: Double): TimeSpan =
    TimeSpan.FromSeconds.unsafeGet(BigDecimal(s))

  // The table from https://app.shortcut.com/lucuma/story/10747: exposure seconds, ITC
  // coadds, step coadds.  round(5 / exposure) is the specified rule, so 3.4s x 1 stays
  // below the floor.
  List(
    (0.2, 1, 25),
    (0.3, 1, 17),
    (3.3, 1,  2),
    (3.4, 1,  1),
    (2.0, 2,  3),
    (2.0, 3,  3),
    (5.0, 1,  1),
    (0.3, 2, 17)
  ).foreach: (exposure, coadds, expected) =>
    test(s"${coadds} x ${exposure}s integrates for at least 5s with $expected coadds"):
      assertEquals(
        AcquisitionConfig.withMinimumIntegration(seconds(exposure), PosInt.unsafeFrom(coadds)).value,
        expected
      )

  test("a zero exposure keeps its coadds"):
    assertEquals(AcquisitionConfig.withMinimumIntegration(TimeSpan.Zero, PosInt.unsafeFrom(2)).value, 2)
