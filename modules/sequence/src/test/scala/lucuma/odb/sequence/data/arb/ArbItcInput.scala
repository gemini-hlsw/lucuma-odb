// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data
package arb

import cats.data.NonEmptyList
import cats.syntax.traverse.*
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbTimestamp.given
import lucuma.itc.client.ImagingParameters
import lucuma.itc.client.SpectroscopyParameters
import lucuma.itc.client.TargetInput
import lucuma.itc.client.arb.ArbIntegrationTimeInput.given
import lucuma.itc.client.arb.ArbTargetInput.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits.*

trait ArbItcInput:

  private def genTargetDefinition(id: Long): Gen[ItcInput.TargetDefinition] =
    for
      t <- arbitrary[TargetInput]
      m <- arbitrary[Option[Timestamp]]
    yield ItcInput.TargetDefinition(Target.Id.fromLong(id).get, t, m)

  given Arbitrary[ItcInput.Imaging] =
    Arbitrary:
      for
        cp <- Gen.choose(1, 10)
        ss <- Gen.listOfN(cp, arbitrary[ImagingParameters])
        ct <- Gen.choose(1, 10)
        ts <- List.range(1L, ct + 1L).traverse(genTargetDefinition)
      yield ItcInput.Imaging(
        NonEmptyList.fromListUnsafe(ss),
        NonEmptyList.fromListUnsafe(ts)
      )

  given Arbitrary[ItcInput.Spectroscopy] =
    Arbitrary:
      for
        acq <- arbitrary[ImagingParameters]
        sci <- arbitrary[SpectroscopyParameters]
        ct  <- Gen.choose(1, 10)
        ts  <- List.range(1L, ct + 1L).traverse(genTargetDefinition)
        bo  <- Gen.option(genTargetDefinition(ct + 1L))
      yield ItcInput.Spectroscopy(
        acq,
        sci,
        NonEmptyList.fromListUnsafe(ts),
        bo
      )

  given Arbitrary[ItcInput] =
    Arbitrary:
      Gen.oneOf(arbitrary[ItcInput.Imaging], arbitrary[ItcInput.Spectroscopy])

object ArbItcInput extends ArbItcInput