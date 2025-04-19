// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import lucuma.core.model.sequence.Atom
import lucuma.core.util.Timestamp

class StepRecordPredicates(path: Path) {
  lazy val id         = LeafPredicates[Atom.Id](path / "id")
  lazy val created    = LeafPredicates[Timestamp](path / "created")
  lazy val atomRecord = new AtomRecordPredicates(path / "atom")
}
