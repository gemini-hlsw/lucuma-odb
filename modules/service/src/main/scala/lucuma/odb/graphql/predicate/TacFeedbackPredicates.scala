// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Program

class TacFeedbackPredicates(path: Path):
  lazy val programId = LeafPredicates[Program.Id](path / "programId")
  lazy val category  = LeafPredicates[TimeAccountingCategory](path / "category")
