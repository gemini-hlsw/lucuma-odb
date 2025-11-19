// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.odb.graphql.OdbSuite
import lucuma.odb.StartupDiagnostics

class StartupDiagnosticsSuite extends OdbSuite:

  val validUsers = Nil

  // This ensures that diagnostics must pass in CI
  test("run diagnostics"):
    withSession: s =>
      StartupDiagnostics(s).runAllDiagnostics(true)

