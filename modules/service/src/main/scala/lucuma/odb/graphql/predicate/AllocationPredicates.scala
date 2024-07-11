// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import lucuma.core.model.Program

class AllocationPredicates(path: Path) {
  lazy val id = LeafPredicates[Program.Id](path / "programId")
}
