// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input
package arb

import cats.data.NonEmptyList
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.MultipleFiltersMode
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.data.Nullable
import lucuma.odb.data.arb.ArbNullable.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbGmosImagingInput:

  import ArbEnumerated.given
  import ArbOffset.given

  given Arbitrary[GmosImagingInput.Create.Common] =
    Arbitrary {
      for {
        f <- arbitrary[Option[MultipleFiltersMode]]
        b <- arbitrary[Option[GmosBinning]]
        m <- arbitrary[Option[GmosAmpReadMode]]
        g <- arbitrary[Option[GmosAmpGain]]
        r <- arbitrary[Option[GmosRoi]]
        s <- arbitrary[List[Offset]]
      } yield GmosImagingInput.Create.Common(f, b, m, g, r, s)
    }

  given Arbitrary[GmosImagingInput.Create.North] =
    Arbitrary {
      for {
        f1 <- arbitrary[GmosNorthFilter]
        fs <- arbitrary[List[GmosNorthFilter]]
        c <- arbitrary[GmosImagingInput.Create.Common]
      } yield GmosImagingInput.Create.North(NonEmptyList(f1, fs), c)
    }

  given Arbitrary[GmosImagingInput.Create.South] =
    Arbitrary {
      for {
        f1 <- arbitrary[GmosSouthFilter]
        fs <- arbitrary[List[GmosSouthFilter]]
        c <- arbitrary[GmosImagingInput.Create.Common]
      } yield GmosImagingInput.Create.South(NonEmptyList(f1, fs), c)
    }

  given arbEditCommon: Arbitrary[GmosImagingInput.Edit.Common] =
    Arbitrary:
      for {
        f <- arbitrary[Nullable[MultipleFiltersMode]]
        b <- arbitrary[Nullable[GmosBinning]]
        m <- arbitrary[Nullable[GmosAmpReadMode]]
        g <- arbitrary[Nullable[GmosAmpGain]]
        r <- arbitrary[Nullable[GmosRoi]]
        s <- arbitrary[List[Offset]]
      } yield GmosImagingInput.Edit.Common(f, b, m, g, r, s)

  given arbEditCommonN: Arbitrary[GmosImagingInput.Edit.North] =
    Arbitrary {
      for {
        s <- Gen.choose(1, 4)
        f <- Gen.option(Gen.listOfN(s, arbitrary[GmosNorthFilter]).map(NonEmptyList.fromListUnsafe))
        c <- arbitrary[GmosImagingInput.Edit.Common]
      } yield GmosImagingInput.Edit.North(f, c)
    }

  given arbEditCommonS: Arbitrary[GmosImagingInput.Edit.South] =
    Arbitrary {
      for {
        s <- Gen.choose(1, 4)
        f <- Gen.option(Gen.listOfN(s, arbitrary[GmosSouthFilter]).map(NonEmptyList.fromListUnsafe))
        c <- arbitrary[GmosImagingInput.Edit.Common]
      } yield GmosImagingInput.Edit.South(f, c)
    }

object ArbGmosImagingInput extends ArbGmosImagingInput
