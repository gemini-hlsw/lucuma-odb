// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import lucuma.core.model.Group
import lucuma.core.model.group.GroupConstraintWaiver

class GroupConstraintWaiverPredicates(path: Path):
  val id        = LeafPredicates[GroupConstraintWaiver.Id](path / "id")
  val groupId   = LeafPredicates[Group.Id](path / "groupId")
  val active    = LeafPredicates[Boolean](path / "active")
