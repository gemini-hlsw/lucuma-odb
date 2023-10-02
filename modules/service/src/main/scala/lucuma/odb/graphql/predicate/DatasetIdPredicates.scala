// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosShort
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step

class DatasetIdPredicates(path: Path) {
  lazy val stepId = LeafPredicates[Step.Id](path / "stepId")
  lazy val index  = LeafPredicates[PosShort](path / "index")

  def eql(a: Dataset.Id): Predicate =
    And(stepId.eql(a.stepId), index.eql(PosShort.unsafeFrom(a.index.value)))


}
