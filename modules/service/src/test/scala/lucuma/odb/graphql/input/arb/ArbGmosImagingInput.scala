// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input
package arb

import cats.Order
import cats.data.NonEmptyList
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.arb.ArbExposureTimeMode
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.data.Nullable
import lucuma.odb.data.arb.ArbNullable.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbGmosImagingInput:

  import ArbEnumerated.given
  import ArbExposureTimeMode.given
  import ArbGmosImagingVariantInput.given

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

  given Arbitrary[GmosImagingInput.Create.North] =
    Arbitrary:
      for
        v <- arbitrary[GmosImagingVariantInput]
        f <- genFilterList[GmosImagingFilterInput[GmosNorthFilter], GmosNorthFilter](_.filter)
        c <- arbitrary[GmosImagingInput.Create.Common]
      yield GmosImagingInput.Create(v, f, c)

  given Arbitrary[GmosImagingInput.Create.South] =
    Arbitrary:
      for
        v <- arbitrary[GmosImagingVariantInput]
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