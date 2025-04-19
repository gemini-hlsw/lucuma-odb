// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import eu.timepit.refined.types.numeric.NonNegShort
import grackle.Path
import lucuma.core.model.Group

class GroupElementPredicates(path: Path) {
  lazy val parentGroupId = LeafPredicates[Group.Id](path / "parentGroupId")
  lazy val parentIndex = LeafPredicates[NonNegShort](path / "parentIndex")
  lazy val group = GroupPredicates(path / "group")
  lazy val observation = ObservationPredicates(path / "group")
  lazy val existence = ExistencePredicates(path / "existence")

}
