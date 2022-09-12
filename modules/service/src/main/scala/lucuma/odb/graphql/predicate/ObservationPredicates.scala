// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicates

import edu.gemini.grackle.Path.ListPath
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate.*
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model.Access.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.Existence

class ObservationPredicates(path: List[String]) {
  lazy val existence = ExistencePredicates(path :+ "existence")
  lazy val id = LeafPredicates[Observation.Id](path :+ "id")
  lazy val program = new ProgramPredicates(path :+ "program")
}
