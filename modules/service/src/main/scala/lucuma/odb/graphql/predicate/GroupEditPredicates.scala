// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path

class GroupEditPredicates(path: Path) {
  val value = GroupPredicates(path / "value")
  val program = ProgramPredicates(path / "program")
}
