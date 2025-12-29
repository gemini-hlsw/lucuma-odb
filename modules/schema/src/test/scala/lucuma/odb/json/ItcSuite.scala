// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.odb.data.Itc
import lucuma.odb.data.arb.ArbItc
import munit.DisciplineSuite

class ItcSuite extends DisciplineSuite with ArbitraryInstances:
  import ArbItc.given
  import itc.given
  import time.query.given
  import wavelength.query.given

  checkAll("ItcCodec", CodecTests[Itc].codec)