// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.data.Zipper
import lucuma.core.data.arb.ArbZipper
import munit.DisciplineSuite

class ZipperSuite extends DisciplineSuite with ArbitraryInstances {

  import ArbZipper.given
  import zipper.given

  checkAll("Zipper", CodecTests[Zipper[Int]].codec)

}