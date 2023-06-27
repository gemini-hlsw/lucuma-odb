// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import edu.gemini.grackle.Path

class TargetGroupPredicates(path: Path) {
  val program = ProgramPredicates(path / "program")
  val observations = ObservationSelectResultPredicates(path / "observations")
  val target = TargetPredicates(path / "target")
}
