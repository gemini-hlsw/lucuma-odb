// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.data.NonEmptyList
import cats.syntax.eq.*
import cats.syntax.functor.*
import lucuma.core.model.Target
import lucuma.itc.client.ImagingParameters
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.SpectroscopyParameters
import lucuma.itc.client.TargetInput
import lucuma.itc.client.arb.ArbInstrumentMode.given
import lucuma.itc.client.arb.ArbIntegrationTimeInput.given
import lucuma.itc.client.arb.ArbTargetInput.given
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class ItcInputSuite extends ScalaCheckSuite:

  given Arbitrary[ItcInput] =
    Arbitrary:
      for
        m <- arbitrary[InstrumentMode]
        a <- arbitrary[Option[ImagingParameters]]
        s <- arbitrary[SpectroscopyParameters]
        c <- Gen.choose(1, 10)
        t <- Gen.listOfN(c, arbitrary[TargetInput])
        b <- arbitrary[Option[TargetInput]]
      yield
        val targetIds = (1 to c).map(i => Target.Id.fromLong(i.toLong).get)
        val targets   = NonEmptyList.fromListUnsafe(targetIds.zip(t).map((id, target) => (id, target, None)).toList)

        ItcInput(
          a.map(_.copy(mode = m)),
          List.empty,
          List(s.copy(mode = m)),
          targets,
          b.map(input => (Target.Id.fromLong((c + 1).toLong).get, input, None))
        )

  property("acquisition sequence uses blind offset target when available, science sequence never uses blind offset"):
    forAll { (itcInput: ItcInput) =>
      val acquisitionInput = itcInput.acquisitionInput
      val scienceInputs    = itcInput.spectroscopyInputs

      // Acquisition should use blind offset target only.
      val bot = itcInput.blindOffsetTarget.map(_._2)
      acquisitionInput.foreach: acq =>
        assertEquals(acq.asterism.length, bot.as(1).getOrElse(itcInput.targets.length))
        assertEquals(acq.asterism.head,   bot.getOrElse(itcInput.targets.head._2))

      // Science should use regular targets only
      assert(scienceInputs.forall(_.asterism.length === itcInput.targets.size))
      assert(scienceInputs.forall(_.asterism.head === itcInput.targets.head._2))
    }