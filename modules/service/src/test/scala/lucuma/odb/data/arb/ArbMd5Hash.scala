// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data
package arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbMd5Hash {
  given Arbitrary[Md5Hash] =
    Arbitrary {
      for {
        l <- Gen.listOfN(16, arbitrary[Byte])
      } yield Md5Hash.unsafeFromByteArray(l.toArray)
    }

  given Cogen[Md5Hash] =
    Cogen[Array[Byte]].contramap(_.toByteArray)
}

object ArbMd5Hash extends ArbMd5Hash
