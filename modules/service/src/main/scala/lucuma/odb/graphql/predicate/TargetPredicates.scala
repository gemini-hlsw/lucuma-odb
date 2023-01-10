// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model.Target
import lucuma.odb.data.Existence

class TargetPredicates(path: Path) {
  lazy val existence = ExistencePredicates(path / "existence")
  lazy val id = LeafPredicates[Target.Id](path / "id")
  lazy val program = ProgramPredicates(path / "program")
}

