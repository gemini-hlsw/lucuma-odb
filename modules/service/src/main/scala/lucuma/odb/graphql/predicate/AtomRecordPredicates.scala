// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package predicate

import grackle.Path
import lucuma.core.model.sequence.Atom
import lucuma.core.util.Timestamp

class AtomRecordPredicates(path: Path) {
  lazy val id      = LeafPredicates[Atom.Id](path / "id")
  lazy val created = LeafPredicates[Timestamp](path / "created")
  lazy val visit   = new VisitPredicates(path / "visit")
}
