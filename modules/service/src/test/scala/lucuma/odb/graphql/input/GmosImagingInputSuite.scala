// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import io.circe.testing.ArbitraryInstances
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosRoi
import munit.DisciplineSuite
import munit.FunSuite

class GmosImagingInputSuite extends DisciplineSuite with ArbitraryInstances :

  test("GmosImagingInput.Create.Common stores explicit values correctly") {
    val common = GmosImagingInput.Create.Common(
      explicitBin = Some(GmosBinning.Two),
      explicitAmpReadMode = Some(GmosAmpReadMode.Slow),
      explicitAmpGain = Some(GmosAmpGain.Low),
      explicitRoi = Some(GmosRoi.FullFrame)
    )

    assertEquals(common.explicitBin, Some(GmosBinning.Two))
    assertEquals(common.explicitAmpReadMode, Some(GmosAmpReadMode.Slow))
    assertEquals(common.explicitAmpGain, Some(GmosAmpGain.Low))
    assertEquals(common.explicitRoi, Some(GmosRoi.FullFrame))
  }

  test("GmosImagingInput.Edit.Common toCreate method works correctly") {
    import lucuma.odb.data.Nullable

    val editCommon = GmosImagingInput.Edit.Common(
      explicitBin = Nullable.NonNull(GmosBinning.Two),
      explicitAmpReadMode = Nullable.NonNull(GmosAmpReadMode.Slow),
      explicitAmpGain = Nullable.Null,
      explicitRoi = Nullable.Absent
    )

    val createCommon = editCommon.toCreate

    assertEquals(createCommon.explicitBin, Some(GmosBinning.Two))
    assertEquals(createCommon.explicitAmpReadMode, Some(GmosAmpReadMode.Slow))
    assertEquals(createCommon.explicitAmpGain, None)
    assertEquals(createCommon.explicitRoi, None)
  }