// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.model.Group

class GroupElementPredicates(path: Path) {
  lazy val parentGroupId = LeafPredicates[Group.Id](path / "parentGroupId")
  lazy val parentIndex = LeafPredicates[NonNegShort](path / "parentIndex")
  lazy val group = GroupPredicates(path / "group")
  lazy val observation = ObservationPredicates(path / "group")
}
