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
import lucuma.odb.data.WavelengthOrder
import lucuma.odb.data.arb.ArbNullable.given
import lucuma.odb.sequence.data.OffsetGenerator
import lucuma.odb.sequence.data.arb.ArbOffsetGenerator
import lucuma.odb.sequence.gmos.imaging.Variant
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbGmosImagingInput:

  import ArbEnumerated.given
  import ArbExposureTimeMode.given
  import ArbOffset.given
  import ArbOffsetGenerator.given

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

  def genGrouped[A: Arbitrary, L: Order](f: A => L): Gen[Variant.Grouped[A]] =
    for
      fs <- genFilterList[A, L](f)
      wo <- arbitrary[WavelengthOrder]
      of <- arbitrary[OffsetGenerator]
      sc <- Gen.choose(0, 100).map(NonNegInt.unsafeFrom)
      so <- arbitrary[OffsetGenerator]
    yield Variant.Grouped(fs, wo, of, sc, so)

  def genInterleaved[A: Arbitrary, L: Order](f: A => L): Gen[Variant.Interleaved[A]] =
    genFilterList[A, L](f).map(Variant.Interleaved.apply)

  def genPreImaging[A: Arbitrary, L: Order](f: A => L): Gen[Variant.PreImaging[A]] =
    for
      fs <- genFilterList[A, L](f)
      o1 <- arbitrary[Offset]
      o2 <- arbitrary[Offset]
      o3 <- arbitrary[Offset]
      o4 <- arbitrary[Offset]
    yield Variant.PreImaging(fs, o1, o2, o3, o4)

  def genVariant[A: Arbitrary, L: Order](f: A => L): Gen[Variant[A]] =
    Gen.oneOf(genGrouped(f), genInterleaved(f), genPreImaging(f))

  given Arbitrary[GmosImagingInput.Create.North] =
    Arbitrary:
      for
        v <- genVariant[GmosImagingFilterInput[GmosNorthFilter], GmosNorthFilter](_.filter)
        c <- arbitrary[GmosImagingInput.Create.Common]
      yield GmosImagingInput.Create(v, c)

  given Arbitrary[GmosImagingInput.Create.South] =
    Arbitrary:
      for
        v <- genVariant[GmosImagingFilterInput[GmosSouthFilter], GmosSouthFilter](_.filter)
        c <- arbitrary[GmosImagingInput.Create.Common]
      yield GmosImagingInput.Create(v, c)

  given arbEditCommon: Arbitrary[GmosImagingInput.Edit.Common] =
    Arbitrary:
      for
        b <- arbitrary[Nullable[GmosBinning]]
        m <- arbitrary[Nullable[GmosAmpReadMode]]
        g <- arbitrary[Nullable[GmosAmpGain]]
        r <- arbitrary[Nullable[GmosRoi]]
      yield GmosImagingInput.Edit.Common(b, m, g, r)

  def genGroupedInput[A: Arbitrary, L: Order](f: A => L): Gen[GmosImagingVariantInput[A]] =
    for
      fs <- Gen.option(genFilterList[A, L](f))
      wo <- arbitrary[Option[WavelengthOrder]]
      of <- arbitrary[Nullable[OffsetGenerator]]
      sc <- Gen.option(Gen.choose(0, 100).map(NonNegInt.unsafeFrom))
      so <- arbitrary[Nullable[OffsetGenerator]]
    yield GmosImagingVariantInput.Grouped(fs, wo, of, sc, so)

  def genInterleavedInput[A: Arbitrary, L: Order](f: A => L): Gen[GmosImagingVariantInput[A]] =
      Gen.option(genFilterList[A, L](f)).map(GmosImagingVariantInput.Interleaved.apply)

  def genPreImagingInput[A: Arbitrary, L: Order](f: A => L): Gen[GmosImagingVariantInput[A]] =
    for
      fs <- Gen.option(genFilterList[A, L](f))
      o1 <- arbitrary[Option[Offset]]
      o2 <- arbitrary[Option[Offset]]
      o3 <- arbitrary[Option[Offset]]
      o4 <- arbitrary[Option[Offset]]
    yield GmosImagingVariantInput.PreImaging(fs, o1, o2, o3, o4)

  def genVariantInput[A: Arbitrary, L: Order](f: A => L): Gen[GmosImagingVariantInput[A]] =
    Gen.oneOf(genGroupedInput(f), genInterleavedInput(f), genPreImagingInput(f))

  given arbEditCommonN: Arbitrary[GmosImagingInput.Edit.North] =
    Arbitrary:
      for
        v <- Gen.option(genVariantInput[GmosImagingFilterInput[GmosNorthFilter], GmosNorthFilter](_.filter))
        c <- arbitrary[GmosImagingInput.Edit.Common]
      yield GmosImagingInput.Edit(v, c)

  given arbEditCommonS: Arbitrary[GmosImagingInput.Edit.South] =
    Arbitrary:
      for
        v <- Gen.option(genVariantInput[GmosImagingFilterInput[GmosSouthFilter], GmosSouthFilter](_.filter))
        c <- arbitrary[GmosImagingInput.Edit.Common]
      yield GmosImagingInput.Edit(v, c)

object ArbGmosImagingInput extends ArbGmosImagingInput