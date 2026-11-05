// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.enums.CalibrationRole
import munit.FunSuite

class CalibrationRolesSuite extends FunSuite:

  test("per-observation and per-program roles are disjoint"):
    val both = ObsExtract.PerObservationCalibrationRoles.toSet.intersect(ObsExtract.PerProgramPerConfigCalibrationTypes.toSet)
    assertEquals(both, Set.empty[CalibrationRole])
