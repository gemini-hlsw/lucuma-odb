// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input
package arb

import cats.Order
import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.arb.ArbExposureTimeMode
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.data.Nullable
import lucuma.core.enums.WavelengthOrder
import lucuma.odb.data.arb.ArbNullable.given
import lucuma.odb.sequence.data.TelescopeConfigGenerator
import lucuma.odb.sequence.data.arb.ArbTelescopeConfigGenerator
import lucuma.odb.sequence.gmos.imaging.Variant
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbGmosImagingInput:

  import ArbEnumerated.given
  import ArbExposureTimeMode.given
  import ArbOffset.given
  import ArbTelescopeConfigGenerator.given

  given [L: Arbitrary]: Arbitrary[GmosImagingFilterInput[L]] =
    Arbitrary:
      for
        f <- arbitrary[L]
        e <- arbitrary[Option[ExposureTimeMode]]
      yield GmosImagingFilterInput(f, e)

  given Arbitrary[GmosImagingInput.Create.Common] =
    Arbitrary:
      for
        b <- arbitrary[Option[GmosBinning]]
        m <- arbitrary[Option[GmosAmpReadMode]]
        g <- arbitrary[Option[GmosAmpGain]]
        r <- arbitrary[Option[GmosRoi]]
      yield GmosImagingInput.Create.Common(b, m, g, r)

  private def genFilterList[A: Arbitrary, L: Order](f: A => L): Gen[NonEmptyList[A]] =
    for
      f1 <- arbitrary[A]
      fs <- arbitrary[List[A]]
    yield NonEmptyList.fromListUnsafe(f1 :: fs).distinctBy(f)

  given arbVariantGrouped: Arbitrary[Variant.Grouped] =
    Arbitrary:
      for
        wo <- arbitrary[WavelengthOrder]
        of <- arbitrary[TelescopeConfigGenerator]
        sc <- Gen.choose(0, 100).map(NonNegInt.unsafeFrom)
        so <- arbitrary[TelescopeConfigGenerator]
      yield Variant.Grouped(wo, of, sc, so)

  given arbVariantInterleaved: Arbitrary[Variant.Interleaved] =
    Arbitrary:
      for
        of <- arbitrary[TelescopeConfigGenerator]
        sc <- Gen.choose(0, 100).map(NonNegInt.unsafeFrom)
        so <- arbitrary[TelescopeConfigGenerator]
      yield Variant.Interleaved(of, sc, so)

  given arbVariantPreImaging: Arbitrary[Variant.PreImaging] =
    Arbitrary:
      for
        o1 <- arbitrary[Offset]
        o2 <- arbitrary[Offset]
        o3 <- arbitrary[Offset]
        o4 <- arbitrary[Offset]
      yield Variant.PreImaging(o1, o2, o3, o4)

  given Arbitrary[Variant] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[Variant.Grouped](using arbVariantGrouped),
        arbitrary[Variant.Interleaved](using arbVariantInterleaved),
        arbitrary[Variant.PreImaging](using arbVariantPreImaging)
      )

  given Arbitrary[GmosImagingInput.Create.North] =
    Arbitrary:
      for
        v <- arbitrary[Variant]
        f <- genFilterList[GmosImagingFilterInput[GmosNorthFilter], GmosNorthFilter](_.filter)
        c <- arbitrary[GmosImagingInput.Create.Common]
      yield GmosImagingInput.Create(v, f, c)

  given Arbitrary[GmosImagingInput.Create.South] =
    Arbitrary:
      for
        v <- arbitrary[Variant]
        f <- genFilterList[GmosImagingFilterInput[GmosSouthFilter], GmosSouthFilter](_.filter)
        c <- arbitrary[GmosImagingInput.Create.Common]
      yield GmosImagingInput.Create(v, f, c)

  given arbEditCommon: Arbitrary[GmosImagingInput.Edit.Common] =
    Arbitrary:
      for
        b <- arbitrary[Nullable[GmosBinning]]
        m <- arbitrary[Nullable[GmosAmpReadMode]]
        g <- arbitrary[Nullable[GmosAmpGain]]
        r <- arbitrary[Nullable[GmosRoi]]
      yield GmosImagingInput.Edit.Common(b, m, g, r)

  given arbVariantInputGrouped: Arbitrary[GmosImagingVariantInput.Grouped] =
    Arbitrary:
      for
        wo <- arbitrary[Option[WavelengthOrder]]
        of <- arbitrary[Nullable[TelescopeConfigGenerator]]
        sc <- Gen.option(Gen.choose(0, 100).map(NonNegInt.unsafeFrom))
        so <- arbitrary[Nullable[TelescopeConfigGenerator]]
      yield GmosImagingVariantInput.Grouped(wo, of, sc, so)

  given arbVariantInputInterleaved: Arbitrary[GmosImagingVariantInput.Interleaved] =
    Arbitrary:
      for
        of <- arbitrary[Nullable[TelescopeConfigGenerator]]
        sc <- Gen.option(Gen.choose(0, 100).map(NonNegInt.unsafeFrom))
        so <- arbitrary[Nullable[TelescopeConfigGenerator]]
      yield GmosImagingVariantInput.Interleaved(of, sc, so)

  given arbVariantInputPreImaging: Arbitrary[GmosImagingVariantInput.PreImaging] =
    Arbitrary:
      for
        o1 <- arbitrary[Option[Offset]]
        o2 <- arbitrary[Option[Offset]]
        o3 <- arbitrary[Option[Offset]]
        o4 <- arbitrary[Option[Offset]]
      yield GmosImagingVariantInput.PreImaging(o1, o2, o3, o4)

  given Arbitrary[GmosImagingVariantInput] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[GmosImagingVariantInput.Grouped](using arbVariantInputGrouped),
        arbitrary[GmosImagingVariantInput.Interleaved](using arbVariantInputInterleaved),
        arbitrary[GmosImagingVariantInput.PreImaging](using arbVariantInputPreImaging)
      )

  given arbEditCommonN: Arbitrary[GmosImagingInput.Edit.North] =
    Arbitrary:
      for
        v <- arbitrary[Option[GmosImagingVariantInput]]
        f <- Gen.option(genFilterList[GmosImagingFilterInput[GmosNorthFilter], GmosNorthFilter](_.filter))
        c <- arbitrary[GmosImagingInput.Edit.Common]
      yield GmosImagingInput.Edit(v, f, c)

  given arbEditCommonS: Arbitrary[GmosImagingInput.Edit.South] =
    Arbitrary:
      for
        v <- arbitrary[Option[GmosImagingVariantInput]]
        f <- Gen.option(genFilterList[GmosImagingFilterInput[GmosSouthFilter], GmosSouthFilter](_.filter))
        c <- arbitrary[GmosImagingInput.Edit.Common]
      yield GmosImagingInput.Edit(v, f, c)

object ArbGmosImagingInput extends ArbGmosImagingInput