// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import edu.gemini.grackle.Path
import lucuma.odb.data.Group

class GroupPredicates(path: Path) {
  lazy val id       = LeafPredicates[Group.Id](path / "id")
  lazy val parentId = LeafPredicates[Group.Id](path / "parentId")
}
