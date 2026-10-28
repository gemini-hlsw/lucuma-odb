// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package imaging

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.sequence.data.TelescopeConfigGenerator
import lucuma.odb.sequence.syntax.hash.*

class VariantSuite extends munit.FunSuite:

  private val offset: Offset =
    Offset(Offset.P.signedDecimalArcseconds.reverseGet(BigDecimal(1)), Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal(2)))

  private val gen: TelescopeConfigGenerator =
    TelescopeConfigGenerator.Enumerated(NonEmptyList.of(TelescopeConfig(offset, StepGuideState.Enabled)))

  private val grouped: Variant.Grouped =
    Variant.Grouped(WavelengthOrder.Decreasing, gen, NonNegInt.unsafeFrom(2), TelescopeConfigGenerator.NoGenerator, PosInt.MinValue)

  private val interleaved: Variant.Interleaved =
    Variant.Interleaved(gen, NonNegInt.unsafeFrom(2), TelescopeConfigGenerator.NoGenerator, PosInt.MinValue)

  private val preImaging: Variant.PreImaging =
    Variant.PreImaging(offset, Offset.Zero, offset, Offset.Zero, PosInt.MinValue)

  private def bytes(v: Variant): List[Byte] =
    (v: Variant).hashBytes.toList

  // The hash of each variant as written before exposuresPerOffset existed.  With
  // the default of 1 it must not change, since the guide star override hash is
  // computed from it and a change would discard manually selected guide stars.

  test("grouped hash with one exposure per offset is unchanged"):
    assertEquals(
      bytes(grouped),
      Array.concat(grouped.order.hashBytes, grouped.offsets.hashBytes, grouped.skyCount.hashBytes, grouped.skyOffsets.hashBytes).toList
    )

  test("interleaved hash with one exposure per offset is unchanged"):
    assertEquals(
      bytes(interleaved),
      Array.concat(interleaved.offsets.hashBytes, interleaved.skyCount.hashBytes, interleaved.skyOffsets.hashBytes).toList
    )

  test("preImaging hash with one exposure per offset is unchanged"):
    assertEquals(
      bytes(preImaging),
      Array.concat(preImaging.offset1.hashBytes, preImaging.offset2.hashBytes, preImaging.offset3.hashBytes, preImaging.offset4.hashBytes).toList
    )

  test("exposuresPerOffset other than one changes the hash"):
    val k = PosInt.unsafeFrom(3)
    List(grouped, interleaved, preImaging).foreach: v =>
      assertNotEquals(bytes(Variant.exposuresPerOffset.replace(k)(v)), bytes(v))

  test("exposuresPerOffset lens"):
    val k = PosInt.unsafeFrom(3)
    List(grouped, interleaved, preImaging).foreach: v =>
      val vʹ = Variant.exposuresPerOffset.replace(k)(v)
      assertEquals(Variant.exposuresPerOffset.get(vʹ), k)
      assertEquals(vʹ.variantType, v.variantType)
