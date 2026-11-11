// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource

import cats.effect.IO
import lucuma.resource.test.ServerFixtures
import resource.server.http4s.ResourceMain

class StartupDiagnosticsSuite extends ServerFixtures:

  // This ensures that diagnostics must pass in CI
  test("diagnostics pass against the clean schema"):
    ResourceMain.runStartupDiagnostics[IO](databaseConfig)
