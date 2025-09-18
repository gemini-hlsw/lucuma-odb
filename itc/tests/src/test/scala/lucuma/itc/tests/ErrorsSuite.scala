// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.kernel.laws.discipline.EqTests
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.itc.arb.ArbError.given

class ErrorsSuite extends munit.DisciplineSuite with ArbitraryInstances:
  checkAll("Error", EqTests[Error].eqv)
  checkAll("Codec[Error]", CodecTests[Error].codec)
