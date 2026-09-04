// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.syntax.eq.*
import cats.syntax.functor.*
import lucuma.itc.client.TargetInput
import lucuma.odb.sequence.data.arb.ArbItcInput.given
import lucuma.odb.sequence.util.HashBytes
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class ItcInputSuite extends ScalaCheckSuite:

  property("acquisition sequence uses blind offset target when available, science sequence never uses blind offset"):
    forAll { (itcInput: ItcInput.Spectroscopy) =>
      val acq = itcInput.acquisitionInput
      val sci = itcInput.scienceInput

      // Acquisition should use blind offset target only.
      val bot = itcInput.blindOffset.map(_._2)
      assertEquals(acq.asterism.length, bot.as(1).getOrElse(itcInput.targets.length))
      assertEquals(acq.asterism.head,   bot.getOrElse(itcInput.targets.head._2))

      // Science should use regular targets only
      assert(sci.asterism.length === itcInput.targets.size)
      assert(sci.asterism.head === itcInput.targets.head._2)
    }

  property("GNIRS spectroscopy hashes every central wavelength"):
    forAll { (itcInput: ItcInput.GnirsSpectroscopy) =>
      // Dropping a central wavelength must change the cache key: two observations
      // differing only in an extra wavelength must not collide in t_itc_result.
      val trimmed = itcInput.science.toList match
        case _ :: (t @ _ :: _) => Some(itcInput.copy(science = cats.data.NonEmptyList.fromListUnsafe(t)))
        case _                 => None

      trimmed.foreach: t =>
        assert(
          !java.util.Arrays.equals(HashBytes[ItcInput].hashBytes(itcInput), HashBytes[ItcInput].hashBytes(t)),
          "Removing a central wavelength did not change the hash"
        )
    }
