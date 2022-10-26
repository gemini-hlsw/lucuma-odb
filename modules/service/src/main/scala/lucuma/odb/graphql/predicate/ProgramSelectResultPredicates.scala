// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model.Access._
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Existence

class ProgramSelectResultPredicates(path: Path) {

  lazy val matches = ProgramPredicates(path / "matches")
  lazy val hasMore = LeafPredicates[Boolean](path / "hasMore")

}
