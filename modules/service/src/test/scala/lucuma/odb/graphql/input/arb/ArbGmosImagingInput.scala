// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input
package arb

import cats.data.NonEmptyList
import cats.syntax.option.*
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
import lucuma.odb.sequence.gmos.imaging.Variant
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbGmosImagingInput:

  import ArbEnumerated.given
  import ArbExposureTimeMode.given

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

  given Arbitrary[GmosImagingInput.Create.North] =
    Arbitrary:
      for
        f1 <- arbitrary[GmosImagingFilterInput[GmosNorthFilter]]
        fs <- arbitrary[List[GmosImagingFilterInput[GmosNorthFilter]]]
        c <- arbitrary[GmosImagingInput.Create.Common]
      yield GmosImagingInput.Create(Variant.Grouped.default(NonEmptyList(f1, fs)), c)

  given Arbitrary[GmosImagingInput.Create.South] =
    Arbitrary:
      for
        f1 <- arbitrary[GmosImagingFilterInput[GmosSouthFilter]]
        fs <- arbitrary[List[GmosImagingFilterInput[GmosSouthFilter]]]
        c  <- arbitrary[GmosImagingInput.Create.Common]
      yield GmosImagingInput.Create(Variant.Grouped.default(NonEmptyList(f1, fs)), c)

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
        s <- Gen.choose(1, 4)
        f <- Gen.option(Gen.listOfN(s, arbitrary[GmosImagingFilterInput[GmosNorthFilter]]).map(NonEmptyList.fromListUnsafe))
        c <- arbitrary[GmosImagingInput.Edit.Common]
      yield GmosImagingInput.Edit(GmosImagingVariantInput.Interleaved(f).some, c)

  given arbEditCommonS: Arbitrary[GmosImagingInput.Edit.South] =
    Arbitrary:
      for
        s <- Gen.choose(1, 4)
        f <- Gen.option(Gen.listOfN(s, arbitrary[GmosImagingFilterInput[GmosSouthFilter]]).map(NonEmptyList.fromListUnsafe))
        c <- arbitrary[GmosImagingInput.Edit.Common]
      yield GmosImagingInput.Edit(GmosImagingVariantInput.Interleaved(f).some, c)

object ArbGmosImagingInput extends ArbGmosImagingInput