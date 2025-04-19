// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbTarget.given
import munit.DisciplineSuite
import org.scalacheck.Prop
import org.scalacheck.Prop.*

abstract class TargetSuite(using Encoder[Target]) extends DisciplineSuite with ArbitraryInstances {
  import target.decoder.given

  checkAll("TargetCodec", CodecTests[Target].codec)

  // Keys that only the query encoder should always create (not testing optional members at this time)
  private val conversionKeys = List(("ra", "degrees"), ("dec", "dms"))

  // test whether the target contains all of the various unit conversions
  def detailsTest(hasDetails: Boolean): Prop =
    forAll { (s: Target.Sidereal) =>
      Encoder[Target].apply(s).asObject.flatMap(_("sidereal")).flatMap(_.asObject).exists { sidereal =>
        conversionKeys.forall { case (key1, key2) =>
          sidereal(key1).flatMap(_.asObject).flatMap(_(key2)).fold(!hasDetails)(_ => hasDetails)
        }
      }
    }

}

class TargetQuerySuite extends TargetSuite(using target.query.Encoder_Target) {
  test("'query' target includes the unit conversions") {
    detailsTest(true)
  }
}

class TargetTransportSuite extends TargetSuite(using target.transport.Encoder_Target) {
  test("'transport' target does not include the unit conversions") {
    detailsTest(false)
  }
}
