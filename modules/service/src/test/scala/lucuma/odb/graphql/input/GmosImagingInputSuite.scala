// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import io.circe.testing.ArbitraryInstances
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.MultipleFiltersMode
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Offset
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.input.arb.ArbGmosImagingInput.given
import munit.DisciplineSuite
import munit.FunSuite
import org.scalacheck.Prop.forAll

class GmosImagingInputSuite extends DisciplineSuite with ArbitraryInstances :
  test("GmosImagingInput.Create.North should have correct observingModeType"):
    forAll: (c: GmosImagingInput.Create.North) =>
      assertEquals(c.observingModeType, ObservingModeType.GmosNorthImaging)

  test("GmosImagingInput.Create.South should have correct observingModeType"):
    forAll: (c: GmosImagingInput.Create.South) =>
      assertEquals(c.observingModeType, ObservingModeType.GmosSouthImaging)

  test("GmosImagingInput.Create.Common stores explicit values correctly") {
    val common = GmosImagingInput.Create.Common(
      explicitMultipleFiltersMode = Some(MultipleFiltersMode.Interleaved),
      explicitBin = Some(GmosBinning.Two),
      explicitAmpReadMode = Some(GmosAmpReadMode.Slow),
      explicitAmpGain = Some(GmosAmpGain.Low),
      explicitRoi = Some(GmosRoi.FullFrame),
      offsets = Nil
    )

    assertEquals(common.explicitMultipleFiltersMode, Some(MultipleFiltersMode.Interleaved))
    assertEquals(common.explicitBin, Some(GmosBinning.Two))
    assertEquals(common.explicitAmpReadMode, Some(GmosAmpReadMode.Slow))
    assertEquals(common.explicitAmpGain, Some(GmosAmpGain.Low))
    assertEquals(common.explicitRoi, Some(GmosRoi.FullFrame))
  }

  test("GmosImagingInput.Edit.Common toCreate method works correctly") {
    import lucuma.odb.data.Nullable

    val editCommon = GmosImagingInput.Edit.Common(
      explicitMultipleFiltersMode = Nullable.NonNull(MultipleFiltersMode.Interleaved),
      explicitBin = Nullable.NonNull(GmosBinning.Two),
      explicitAmpReadMode = Nullable.NonNull(GmosAmpReadMode.Slow),
      explicitAmpGain = Nullable.Null,
      explicitRoi = Nullable.Absent,
      offsets = List.empty
    )

    val createCommon = editCommon.toCreate

    assertEquals(createCommon.explicitMultipleFiltersMode, Some(MultipleFiltersMode.Interleaved))
    assertEquals(createCommon.explicitBin, Some(GmosBinning.Two))
    assertEquals(createCommon.explicitAmpReadMode, Some(GmosAmpReadMode.Slow))
    assertEquals(createCommon.explicitAmpGain, None)
    assertEquals(createCommon.explicitRoi, None)
  }

  test("SpatialOffsetsFormat can round-trip spatial offsets") {
    val offsets = List(
      Offset(
        Offset.P.signedDecimalArcseconds.reverseGet(BigDecimal("1.5")),
        Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal("2.0"))
      ),
      Offset(
        Offset.P.signedDecimalArcseconds.reverseGet(BigDecimal("-0.5")),
        Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal("1.0"))
      )
    )

    val formatted = OffsetsFormat.reverseGet(offsets)
    val parsed = OffsetsFormat.getOption(formatted)

    assertEquals(parsed, Some(offsets))
  }

  test("SpatialOffsetsFormat formattedSpatialOffsets works in Create.Common") {
    val offsets = List(
      Offset(
        Offset.P.signedDecimalArcseconds.reverseGet(BigDecimal("1.5")),
        Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal("2.0"))
      )
    )

    val common = GmosImagingInput.Create.Common(
      explicitMultipleFiltersMode = None,
      explicitBin = None,
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None,
      offsets = offsets
    )

    assertEquals(common.formattedOffsets, "1.500000,2.000000")
  }
