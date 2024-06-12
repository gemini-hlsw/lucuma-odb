// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import lucuma.core.model.Target

class TargetPredicates(path: Path) {
  lazy val existence = ExistencePredicates(path / "existence")
  lazy val id = LeafPredicates[Target.Id](path / "id")
  lazy val program = ProgramPredicates(path / "program")

  // def hasRole(role: TargetRole): Predicate =
  //   Eql(path / "role", Const[TargetRole](role))
}

