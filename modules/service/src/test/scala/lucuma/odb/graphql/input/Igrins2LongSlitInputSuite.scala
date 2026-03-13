// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import io.circe.testing.ArbitraryInstances
import lucuma.core.math.Offset
import munit.DisciplineSuite

class Igrins2LongSlitInputSuite extends DisciplineSuite with ArbitraryInstances:

  test("OffsetsFormat formattedOffsets works in Create"):
    val offsets = List(
      Offset.Zero.copy(q = Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal(5))),
      Offset.Zero.copy(q = Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal(10)))
    )

    val create = Igrins2LongSlitInput.Create(
      exposureTimeMode = None,
      explicitOffsets = Some(offsets)
    )

    assertEquals(create.formattedOffsets, Some("0.000000,5.000000,0.000000,10.000000"))

  test("OffsetsFormat formattedOffsets works in Edit"):
    val offsets = List(
      Offset.Zero.copy(q = Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal(5))),
      Offset.Zero.copy(q = Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal(10)))
    )

    val edit = Igrins2LongSlitInput.Edit(
      exposureTimeMode = None,
      explicitOffsetMode = lucuma.odb.data.Nullable.Absent,
      explicitSaveSVCImages = lucuma.odb.data.Nullable.Absent,
      explicitOffsets = lucuma.odb.data.Nullable.NonNull(offsets)
    )

    assertEquals(edit.formattedOffsets, lucuma.odb.data.Nullable.NonNull("0.000000,5.000000,0.000000,10.000000"))
