// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.kernel.laws.discipline.EqTests
import lucuma.odb.data.arb.ArbMd5Hash
import munit.DisciplineSuite

class Md5HashSuite extends DisciplineSuite {
  import ArbMd5Hash.given

  checkAll("Eq", EqTests[Md5Hash].eqv)  
}
