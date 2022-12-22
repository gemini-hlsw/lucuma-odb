// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model.Target
import lucuma.odb.data.Existence

class AsterismGroupPredicates(path: Path) {
  val program = ProgramPredicates(path / "program")
  val observations = ObservationSelectResultPredicates(path / "observations")
}

