// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input
package arb

import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.data.Nullable
import lucuma.odb.data.arb.ArbNullable
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbImagingVariantInput:

  import ArbEnumerated.given
  import ArbNullable.given
  import ArbOffset.given
  import ArbTelescopeConfigGeneratorInput.given

  given arbVariantInputGrouped: Arbitrary[ImagingVariantInput.Grouped] =
    Arbitrary:
      for
        wo <- arbitrary[Option[WavelengthOrder]]
        of <- arbitrary[Nullable[TelescopeConfigGeneratorInput]]
        sc <- Gen.option(Gen.choose(0, 100).map(NonNegInt.unsafeFrom))
        so <- arbitrary[Nullable[TelescopeConfigGeneratorInput]]
      yield ImagingVariantInput.Grouped(wo, of, sc, so)

  given arbVariantInputInterleaved: Arbitrary[ImagingVariantInput.Interleaved] =
    Arbitrary:
      for
        of <- arbitrary[Nullable[TelescopeConfigGeneratorInput]]
        sc <- Gen.option(Gen.choose(0, 100).map(NonNegInt.unsafeFrom))
        so <- arbitrary[Nullable[TelescopeConfigGeneratorInput]]
      yield ImagingVariantInput.Interleaved(of, sc, so)

  given arbVariantInputPreImaging: Arbitrary[ImagingVariantInput.PreImaging] =
    Arbitrary:
      for
        o1 <- arbitrary[Option[Offset]]
        o2 <- arbitrary[Option[Offset]]
        o3 <- arbitrary[Option[Offset]]
        o4 <- arbitrary[Option[Offset]]
      yield ImagingVariantInput.PreImaging(o1, o2, o3, o4)

  given Arbitrary[ImagingVariantInput] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[ImagingVariantInput.Grouped](using arbVariantInputGrouped),
        arbitrary[ImagingVariantInput.Interleaved](using arbVariantInputInterleaved),
        arbitrary[ImagingVariantInput.PreImaging](using arbVariantInputPreImaging)
      )

object ArbImagingVariantInput extends ArbImagingVariantInput
