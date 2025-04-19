// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import lucuma.core.model.Group

class GroupPredicates(path: Path) {
  lazy val id        = LeafPredicates[Group.Id](path / "id")
  lazy val parentId  = LeafPredicates[Group.Id](path / "parentId")
  lazy val program   = ProgramPredicates(path / "program")
  lazy val existence = ExistencePredicates(path / "existence")
  lazy val system    = LeafPredicates[Boolean](path / "system")
}
