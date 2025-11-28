// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.semigroup.*
import lucuma.odb.StartupDiagnostics
import lucuma.odb.graphql.OdbMapping
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.graphql.util.SchemaSemigroup

class StartupDiagnosticsSuite extends OdbSuite with SchemaSemigroup:

  val validUsers = Nil

  // This ensures that diagnostics must pass in CI
  test("run diagnostics"):
    withSession: db =>
      for
        es <- Enums.load(db)
        s   = OdbMapping.unsafeLoadOdbSchema |+| es.schema
        _  <- StartupDiagnostics(db, s, es).runAllDiagnostics(true)
      yield ()

