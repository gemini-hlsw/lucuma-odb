// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import cats.kernel.laws.discipline.*
import lucuma.core.optics.laws.discipline.*
import lucuma.odb.sequence.util.arb.ArbCommitHash

final class CommitHashSuite extends munit.DisciplineSuite {

  import ArbCommitHash.given
  import ArbCommitHash.stringsCommitHash

  checkAll("CommitHash", OrderTests[CommitHash].order)
  checkAll("CommitHash.FromString", FormatTests(CommitHash.FromString).formatWith(stringsCommitHash))

}
