// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import lucuma.odb.data.ProgramReference

class ProgramReferencePredicates(path: Path) {
  val label = LeafPredicates[ProgramReference](path / "label")
}
