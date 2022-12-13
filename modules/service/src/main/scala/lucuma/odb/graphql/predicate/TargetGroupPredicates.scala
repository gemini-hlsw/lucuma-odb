// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import edu.gemini.grackle.Path
import lucuma.core.model.Program

class TargetGroupPredicates(path: Path) {
  val programId = LeafPredicates[Program.Id](path / "programId")
  val observations = ObservationSelectResultPredicates(path / "observations")
}
